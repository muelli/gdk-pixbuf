/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/* GdkPixbuf library - GIF image loader
 *
 * Copyright (C) 1999 Mark Crichton
 * Copyright (C) 1999 The Free Software Foundation
 *
 * Authors: Jonathan Blandford <jrb@redhat.com>
 *          Adapted from the gimp gif filter written by Adam Moss <adam@gimp.org>
 *          Gimp work based on earlier work.
 *          Permission to relicense under the LGPL obtained.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

/* This loader is very hairy code.
 *
 * The main loop was not designed for incremental loading, so when it was hacked
 * in it got a bit messy.  Basically, every function is written to expect a failed
 * read_gif, and lets you call it again assuming that the bytes are there.
 *
 * Return vals.
 * Unless otherwise specified, these are the return vals for most functions:
 *
 *  0 -> success
 * -1 -> more bytes needed.
 * -2 -> failure; abort the load
 * -3 -> control needs to be passed back to the main loop
 *        \_ (most of the time returning 0 will get this, but not always)
 *
 * >1 -> for functions that get a guchar, the char will be returned.
 *
 * -jrb (11/03/1999)
 */

/*
 * If you have any images that crash this code, please, please let me know and
 * send them to me.
 *                                            <jrb@redhat.com>
 */



#include "config.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "gdk-pixbuf-private.h"
#include "io-gif-animation.h"



#undef DUMP_IMAGE_DETAILS
#undef IO_GIFDEBUG

#define MAXCOLORMAPSIZE  256
#define MAX_LZW_BITS     12

#define INTERLACE          0x40
#define LOCALCOLORMAP      0x80
#define BitSet(byte, bit)  (((byte) & (bit)) == (bit))
#define LM_to_uint(a,b)         (((b)<<8)|(a))

#define G_MINUCHAR          0
#define G_MAXUCHAR          255



typedef unsigned char CMap[3][MAXCOLORMAPSIZE];

/* Possible states we can be in. */
enum {
	GIF_START,
	GIF_GET_COLORMAP,
	GIF_GET_NEXT_STEP,
	GIF_GET_FRAME_INFO,
	GIF_GET_EXTENSION,
	GIF_GET_COLORMAP2,
	GIF_PREPARE_LZW,
	GIF_LZW_FILL_BUFFER,
	GIF_LZW_CLEAR_CODE,
	GIF_GET_LZW,
	GIF_DONE
};


typedef enum _GifResultType
{
  GIF_RESULT_OKAY, /* 0 */
  GIF_RESULT_OKAY_BYTE,
  GIF_RESULT_MORE_BYTES,
  GIF_RESULT_FAILURE,
  GIF_RESULT_TO_MAINLOOP, /* 4 */
} GifResultType;

typedef struct _GifResult {
        GifResultType type;
        int byte_value;
        /* guint more_bytes; */
        char* buffer;
        const char* message;
} GifResult;



typedef struct _Gif89 Gif89;
struct _Gif89
{
	int transparent;
	int delay_time;
	int input_flag;
	int disposal;
};

typedef struct _GifContext GifContext;
struct _GifContext
{
	int state; /* really only relevant for progressive loading */
	unsigned int width;
	unsigned int height;

        gboolean has_global_cmap;

        CMap global_color_map;
        gint global_colormap_size;
        unsigned int global_bit_pixel;
	unsigned int global_color_resolution;
        unsigned int background_index;
        gboolean stop_after_first_frame;

        gboolean frame_cmap_active;
        CMap frame_color_map;
        gint frame_colormap_size;
        unsigned int frame_bit_pixel;

	unsigned int aspect_ratio;
	GdkPixbufGifAnim *animation;
	GdkPixbufFrame *frame;
	Gif89 gif89;

	/* stuff per frame. */
	int frame_len;
	int frame_height;
	int frame_interlace;
	int x_offset;
	int y_offset;

	/* Static read only */
	FILE *file;

	/* progressive read, only. */
	GdkPixbufModuleSizeFunc size_func;
	GdkPixbufModulePreparedFunc prepare_func;
	GdkPixbufModuleUpdatedFunc update_func;
	gpointer user_data;
        guchar *buf;
	size_t ptr;
	size_t size;
	size_t amount_needed;

	/* extension context */
	guchar extension_label;
	guchar extension_flag;
        gboolean in_loop_extension;

	/* get block context */
	guchar block_count;
	guchar block_buf[280];
	gint block_ptr;

	int old_state; /* used by lzw_fill buffer */
	/* get_code context */
	int code_curbit;
	int code_lastbit;
	int code_done;
	int code_last_byte;
	int lzw_code_pending;

	/* lzw context */
	gint lzw_fresh;
	gint lzw_code_size;
	guchar lzw_set_code_size;
	gint lzw_max_code;
	gint lzw_max_code_size;
	gint lzw_firstcode;
	gint lzw_oldcode;
	gint lzw_clear_code;
	gint lzw_end_code;
	gint *lzw_sp;

	gint lzw_table[2][(1 << MAX_LZW_BITS)];
	gint lzw_stack[(1 << (MAX_LZW_BITS)) * 2 + 1];

	/* painting context */
	gint draw_xpos;
	gint draw_ypos;
	gint draw_pass;

        /* error pointer */
        GError **error;
};

/* The buffer must be at least 255 bytes long. */
static int GetDataBlock (GifContext *, unsigned char *);



#ifdef IO_GIFDEBUG
static int count = 0;
#endif

/* Returns TRUE if read is OK,
 * FALSE if more memory is needed. */
static gboolean
gif_read (GifContext *context, guchar *buffer, size_t len)
{
	gboolean retval;
#ifdef IO_GIFDEBUG
	gint i;
#endif
	if (context->file) {
#ifdef IO_GIFDEBUG
		count += len;
		g_print ("Fsize :%d\tcount :%d\t", len, count);
#endif
		retval = (fread(buffer, len, 1, context->file) != 0);

                if (!retval && ferror (context->file)) {
                        gint save_errno = errno;
                        g_set_error (context->error,
                                     G_FILE_ERROR,
                                     g_file_error_from_errno (save_errno),
                                     _("Failure reading GIF: %s"),
                                     g_strerror (save_errno));
                }

#ifdef IO_GIFDEBUG
		if (len < 100) {
			for (i = 0; i < len; i++)
				g_print ("%d ", buffer[i]);
		}
		g_print ("\n");
#endif

		return retval;
	} else {
#ifdef IO_GIFDEBUG
/*  		g_print ("\tlooking for %d bytes.  size == %d, ptr == %d\n", len, context->size, context->ptr); */
#endif
		if ((context->size - context->ptr) >= len) {
#ifdef IO_GIFDEBUG
			count += len;
#endif
			memcpy (buffer, context->buf + context->ptr, len);
			context->ptr += len;
			context->amount_needed = 0;
#ifdef IO_GIFDEBUG
			g_print ("Psize :%d\tcount :%d\t", len, count);
			if (len < 100) {
				for (i = 0; i < len; i++)
					g_print ("%d ", buffer[i]);
			}
			g_print ("\n");
#endif
			return TRUE;
		}
		context->amount_needed = len - (context->size - context->ptr);
	}
	return FALSE;
}

/* Changes the stage to be GIF_GET_COLORMAP */
static void
gif_set_get_colormap (GifContext *context)
{
	context->global_colormap_size = 0;
	context->state = GIF_GET_COLORMAP;
}

static void
gif_set_get_colormap2 (GifContext *context)
{
	context->frame_colormap_size = 0;
	context->state = GIF_GET_COLORMAP2;
}

static GifResult
gif_get_colormap (GifContext *context)
{
	unsigned char rgb[3];

	while (context->global_colormap_size < context->global_bit_pixel) {
		if (!gif_read (context, rgb, sizeof (rgb))) {
                        return (GifResult) {.type = GIF_RESULT_MORE_BYTES};
		}

		context->global_color_map[0][context->global_colormap_size] = rgb[0];
		context->global_color_map[1][context->global_colormap_size] = rgb[1];
		context->global_color_map[2][context->global_colormap_size] = rgb[2];

                if (context->global_colormap_size == context->background_index) {
                        context->animation->bg_red = rgb[0];
                        context->animation->bg_green = rgb[1];
                        context->animation->bg_blue = rgb[2];
                }

		context->global_colormap_size ++;
	}

	return (GifResult) {.type = GIF_RESULT_OKAY};
}


static GifResult
gif_get_colormap2 (GifContext *context)
{
	unsigned char rgb[3];

	while (context->frame_colormap_size < context->frame_bit_pixel) {
		if (!gif_read (context, rgb, sizeof (rgb))) {
			return (GifResult) {.type = GIF_RESULT_MORE_BYTES};
		}

		context->frame_color_map[0][context->frame_colormap_size] = rgb[0];
		context->frame_color_map[1][context->frame_colormap_size] = rgb[1];
		context->frame_color_map[2][context->frame_colormap_size] = rgb[2];

		context->frame_colormap_size ++;
	}

	return (GifResult) {.type = GIF_RESULT_OKAY};;
}

/*
 * FIXME: Adapt comment
 * in order for this function to work, we need to perform some black magic.
 * We want to return -1 to let the calling function know, as before, that it needs
 * more bytes.  If we return 0, we were able to successfully read all block->count bytes.
 * Problem is, we don't want to reread block_count every time, so we check to see if
 * context->block_count is 0 before we read in the function.
 *
 * As a result, context->block_count MUST be 0 the first time the get_data_block is called
 * within a context, and cannot be 0 the second time it's called.
 */

static GifResult
get_data_block (GifContext *context,
		unsigned char *buf,
		gint *empty_block)
{

	if (context->block_count == 0) {
		if (!gif_read (context, &context->block_count, 1)) {
			return (GifResult) {.type = GIF_RESULT_MORE_BYTES};
		}
	}

	if (context->block_count == 0)
		if (empty_block) {
			*empty_block = TRUE;
			return (GifResult) {.type = GIF_RESULT_OKAY};
		}

	if (!gif_read (context, buf, context->block_count)) {
		return (GifResult) {.type = GIF_RESULT_MORE_BYTES};
	}

	return (GifResult) {.type = GIF_RESULT_OKAY};
}

static void
gif_set_get_extension (GifContext *context)
{
	context->state = GIF_GET_EXTENSION;
	context->extension_flag = TRUE;
	context->extension_label = 0;
	context->block_count = 0;
	context->block_ptr = 0;
}

static GifResult
gif_get_extension (GifContext *context)
{
	GifResult retval;
	gint empty_block = FALSE;

	if (context->extension_flag) {
		if (context->extension_label == 0) {
			/* I guess bad things can happen if we have an extension of 0 )-: */
			/* I should look into this sometime */
			if (!gif_read (context, & context->extension_label , 1)) {
				return (GifResult) {.type = GIF_RESULT_MORE_BYTES};
			}
		}

		switch (context->extension_label) {
                case 0xf9:			/* Graphic Control Extension */
                        retval = get_data_block (context, (unsigned char *) context->block_buf, NULL);
			if (retval.type != GIF_RESULT_OKAY)
				return retval;

			if (context->frame == NULL) {
				/* I only want to set the transparency if I haven't
				 * created the frame yet.
                                 */
				context->gif89.disposal = (context->block_buf[0] >> 2) & 0x7;
				context->gif89.input_flag = (context->block_buf[0] >> 1) & 0x1;
				context->gif89.delay_time = LM_to_uint (context->block_buf[1], context->block_buf[2]);

				if ((context->block_buf[0] & 0x1) != 0) {
					context->gif89.transparent = context->block_buf[3];
				} else {
					context->gif89.transparent = -1;
				}
			}

			/* Now we've successfully loaded this one, we continue on our way */
			context->block_count = 0;
			context->extension_flag = FALSE;
			break;
                case 0xff: /* application extension */
                        if (!context->in_loop_extension) {
                                retval = get_data_block (context, (unsigned char *) context->block_buf, NULL);
                                if (retval.type != GIF_RESULT_OKAY)
                                        return retval;
                                if (!strncmp ((gchar *)context->block_buf, "NETSCAPE2.0", 11) ||
                                    !strncmp ((gchar *)context->block_buf, "ANIMEXTS1.0", 11)) {
                                        context->in_loop_extension = TRUE;
                                }
                                context->block_count = 0;
                        }
                        if (context->in_loop_extension) {
                                do {
                                        retval = get_data_block (context, (unsigned char *) context->block_buf, &empty_block);
                                        if (retval.type != GIF_RESULT_OKAY)
                                                return retval;
                                        if (context->block_buf[0] == 0x01) {
                                                context->animation->loop = context->block_buf[1] + (context->block_buf[2] << 8);
                                                if (context->animation->loop != 0)
                                                        context->animation->loop++;
                                        }
                                        context->block_count = 0;
                                }
                                while (!empty_block);
                                context->in_loop_extension = FALSE;
                                context->extension_flag = FALSE;
                                return (GifResult) {.type = GIF_RESULT_OKAY};
                        }
			break;
		default:
			/* Unhandled extension */
			break;
		}
	}
	/* read all blocks, until I get an empty block, in case there was an extension I didn't know about. */
	do {
		retval = get_data_block (context, (unsigned char *) context->block_buf, &empty_block);
		if (retval.type != GIF_RESULT_OKAY)
			return retval;
		context->block_count = 0;
	} while (!empty_block);

	return (GifResult) {.type = GIF_RESULT_OKAY};
}

static int ZeroDataBlock = FALSE;

/* @buf must be at least 255 bytes long. */
static int
GetDataBlock (GifContext *context,
	      unsigned char *buf)
{
/*  	unsigned char count; */

	if (!gif_read (context, &context->block_count, 1)) {
		/*g_message (_("GIF: error in getting DataBlock size\n"));*/
		return -1;
	}

	ZeroDataBlock = context->block_count == 0;

	if ((context->block_count != 0) && (!gif_read (context, buf, context->block_count))) {
		/*g_message (_("GIF: error in reading DataBlock\n"));*/
		return -1;
	}

	return context->block_count;
}


static void
gif_set_lzw_fill_buffer (GifContext *context)
{
	context->block_count = 0;
	context->old_state = context->state;
	context->state = GIF_LZW_FILL_BUFFER;
}

static GifResult
gif_lzw_fill_buffer (GifContext *context)
{
	GifResult retval;

	if (context->code_done) {
		if (context->code_curbit >= context->code_lastbit) {
                        g_set_error_literal (context->error,
                                             GDK_PIXBUF_ERROR,
                                             GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                                             _("GIF file was missing some data (perhaps it was truncated somehow?)"));

			return (GifResult) {.type = GIF_RESULT_FAILURE};
		}
                /* Is this supposed to be an error or what? */
		/* g_message ("trying to read more data after we've done stuff\n"); */
                g_set_error (context->error,
                             GDK_PIXBUF_ERROR,
                             GDK_PIXBUF_ERROR_FAILED,
                             _("Internal error in the GIF loader (%s)"),
                             G_STRLOC);

		return (GifResult) {.type = GIF_RESULT_FAILURE};
	}


	context->block_buf[0] = context->block_buf[context->code_last_byte - 2];
	context->block_buf[1] = context->block_buf[context->code_last_byte - 1];

	retval = get_data_block (context, &context->block_buf[2], NULL);

	if (retval.type == GIF_RESULT_MORE_BYTES)
		return (GifResult) {.type = GIF_RESULT_MORE_BYTES};

	if (context->block_count == 0)
		context->code_done = TRUE;

	context->code_last_byte = 2 + context->block_count;
	context->code_curbit = (context->code_curbit - context->code_lastbit) + 16;
	context->code_lastbit = (2 + context->block_count) * 8;

	context->state = context->old_state;
	return (GifResult) {.type = GIF_RESULT_OKAY};
}


/*
 Is either returning GIF_RESULT_TO_MAINLOOP or GIF_RESULT_OKAY_BYTE
*/
static GifResult
get_code (GifContext *context,
	  int   code_size)
{
	int i, j, ret;

	if ((context->code_curbit + code_size) >= context->code_lastbit){
		gif_set_lzw_fill_buffer (context);
                GifResult result;
                result.type = GIF_RESULT_TO_MAINLOOP;
		return result;
	}

	ret = 0;
	for (i = context->code_curbit, j = 0; j < code_size; ++i, ++j)
		ret |= ((context->block_buf[i / 8] & (1 << (i % 8))) != 0) << j;

	context->code_curbit += code_size;

	GifResult result;
    result.type = GIF_RESULT_OKAY_BYTE;
    result.byte_value = ret;
    g_assert (ret >= 0);
    printf ("get_code returns: %d\n", ret);
    return result;
}


static void
set_gif_lzw_clear_code (GifContext *context)
{
	context->state = GIF_LZW_CLEAR_CODE;
	context->lzw_code_pending = -1;
}

static GifResult
gif_lzw_clear_code (GifContext *context)
{
	GifResult result;

	result = get_code (context, context->lzw_code_size);
	if (result.type == GIF_RESULT_TO_MAINLOOP) {
        return (GifResult) {.type = GIF_RESULT_OKAY};
    }

    g_assert (result.type == GIF_RESULT_OKAY_BYTE);
	context->lzw_firstcode = context->lzw_oldcode = result.byte_value;
	context->lzw_code_pending = result.byte_value;
	context->state = GIF_GET_LZW;
    return (GifResult) {.type = GIF_RESULT_OKAY};
}

#define CHECK_LZW_SP() G_STMT_START {                                           \
        if ((guchar *)context->lzw_sp >=                                        \
            (guchar *)context->lzw_stack + sizeof (context->lzw_stack)) {       \
                 g_set_error_literal (context->error,                           \
                                      GDK_PIXBUF_ERROR,                         \
                                      GDK_PIXBUF_ERROR_CORRUPT_IMAGE,           \
                                      _("Stack overflow"));                     \
                GifResult ret = {.type = GIF_RESULT_FAILURE};                   \
                return ret;                                                    \
        }                                                                       \
} G_STMT_END

static GifResult
lzw_read_byte (GifContext *context)
{
    static int counter = 0;
    printf ("calling lzw_read_byte: %d\n", counter++);
	int code, incode;
	/* GifResult retval; */
	register int i;

	if (context->lzw_code_pending != -1) {
	    g_assert (context->lzw_code_pending >= 0);
        GifResult ret = {.type = GIF_RESULT_OKAY_BYTE, .byte_value = context->lzw_code_pending};
		context->lzw_code_pending = -1;
		return ret;
	}

	if (context->lzw_fresh) {
		context->lzw_fresh = FALSE;
		do {
			GifResult res = get_code (context, context->lzw_code_size);
			if (res.type == GIF_RESULT_OKAY_BYTE) {
                context->lzw_firstcode = context->lzw_oldcode = res.byte_value;
            } else {
                g_assert (res.type != GIF_RESULT_OKAY);
                g_assert (res.type == GIF_RESULT_TO_MAINLOOP);
                printf ("Bailing out... %d\n", res.type);
				return res;
			}
		} while (context->lzw_firstcode == context->lzw_clear_code);

                GifResult ret = {.type = GIF_RESULT_OKAY_BYTE, .byte_value = context->lzw_firstcode};
                return ret;
	}

	if (context->lzw_sp > context->lzw_stack) {
                GifResult ret = {.type = GIF_RESULT_OKAY_BYTE, .byte_value = *--(context->lzw_sp)};
                return ret;
	}

        GifResult r;
	//printf ("lzw_code_size: %d\n", context->lzw_code_size);
	while (((r = get_code (context, context->lzw_code_size)).type == GIF_RESULT_OKAY_BYTE)
	    && ((code = r.byte_value) >= 0)) {
		printf ("got code: %d\n", code);
		g_assert (code >= 0);
		if (code == context->lzw_clear_code) {
			for (i = 0; i < context->lzw_clear_code; ++i) {
				context->lzw_table[0][i] = 0;
				context->lzw_table[1][i] = i;
			}
			for (; i < (1 << MAX_LZW_BITS); ++i)
				context->lzw_table[0][i] = context->lzw_table[1][i] = 0;
			context->lzw_code_size = context->lzw_set_code_size + 1;
			context->lzw_max_code_size = 2 * context->lzw_clear_code;
			context->lzw_max_code = context->lzw_clear_code + 2;
			context->lzw_sp = context->lzw_stack;

			set_gif_lzw_clear_code (context);
                        GifResult ret = {.type = GIF_RESULT_TO_MAINLOOP};
			return ret;
		} else if (code == context->lzw_end_code) {
			int count;
			unsigned char buf[260];

                        /*  FIXME - we should handle this case */
                        g_set_error_literal (context->error,
                                             GDK_PIXBUF_ERROR,
                                             GDK_PIXBUF_ERROR_FAILED,
                                             _("GIF image loader cannot understand this image."));
                        GifResult ret = {.type = GIF_RESULT_FAILURE};
                        return ret;

			if (ZeroDataBlock) {
                                GifResult ret = {.type = GIF_RESULT_FAILURE};
                                return ret;
			}

			while ((count = GetDataBlock (context, buf)) > 0)
				;

			if (count != 0) {
				/*g_print (_("GIF: missing EOD in data stream (common occurence)"));*/
                                GifResult ret = {.type = GIF_RESULT_FAILURE};
                                return ret;
			}

        }

		incode = code;

		if (code >= context->lzw_max_code) {
                        CHECK_LZW_SP ();
			*(context->lzw_sp)++ = context->lzw_firstcode;
			printf ("lzw1: setting code to %d\n", code = context->lzw_oldcode);
			code = context->lzw_oldcode;
		}

		while (code >= context->lzw_clear_code) {
                        if (code >= (1 << MAX_LZW_BITS)) {
                                g_set_error_literal (context->error,
                                                     GDK_PIXBUF_ERROR,
                                                     GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                                                     _("Bad code encountered"));
                                GifResult ret = {.type = GIF_RESULT_FAILURE};
                                return ret;
                        }
                        CHECK_LZW_SP ();
			*(context->lzw_sp)++ = context->lzw_table[1][code];

			if (code == context->lzw_table[0][code]) {
                                g_set_error_literal (context->error,
                                                     GDK_PIXBUF_ERROR,
                                                     GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                                                     _("Circular table entry in GIF file"));
                                return (GifResult) {.type = GIF_RESULT_FAILURE};
			}
	    	printf ("lzw: setting code to %d\n", context->lzw_table[0][code]);
			code = context->lzw_table[0][code];
		}

                CHECK_LZW_SP ();
		*(context->lzw_sp)++ = context->lzw_firstcode = context->lzw_table[1][code];

		printf ("lzw max: Setting to: %d\n", context->lzw_max_code);
		if ((code = context->lzw_max_code) < (1 << MAX_LZW_BITS)) {
			context->lzw_table[0][code] = context->lzw_oldcode;
			context->lzw_table[1][code] = context->lzw_firstcode;
			++context->lzw_max_code;
			if ((context->lzw_max_code >= context->lzw_max_code_size) &&
			    (context->lzw_max_code_size < (1 << MAX_LZW_BITS))) {
				context->lzw_max_code_size *= 2;
				++context->lzw_code_size;
			}
		}

		context->lzw_oldcode = incode;

		if (context->lzw_sp > context->lzw_stack) {
                        GifResult ret = {.type = GIF_RESULT_OKAY_BYTE, .byte_value = *--(context->lzw_sp)};
                        return ret;
		}
	}
	printf ("Stopped loop with status: %d\n", r.type);
	// g_assert (r.type != GIF_RESULT_OKAY);
	if (r.type == GIF_RESULT_TO_MAINLOOP) {
	  return r;
	} else if (r.type == GIF_RESULT_OKAY_BYTE) {
 	  return (GifResult) {.type = r.type, .byte_value = code};
	} else {
	  /* We are expecting get_code to return either GIF_RESULT_TO_MAINLOOP or GIF_RESULT_OKAY_BYTE */
          g_assert_not_reached ();
	}
}

static void
gif_set_get_lzw (GifContext *context)
{
	context->state = GIF_GET_LZW;
	context->draw_xpos = 0;
	context->draw_ypos = 0;
	context->draw_pass = 0;
}

static void
gif_fill_in_pixels (GifContext *context, guchar *dest, gint offset, guchar v)
{
	guchar *pixel = NULL;
        guchar (*cmap)[MAXCOLORMAPSIZE];

        if (context->frame_cmap_active)
                cmap = context->frame_color_map;
        else
                cmap = context->global_color_map;

	if (context->gif89.transparent != -1) {
		pixel = dest + (context->draw_ypos + offset) * gdk_pixbuf_get_rowstride (context->frame->pixbuf) + context->draw_xpos * 4;
		*pixel = cmap [0][(guchar) v];
		*(pixel+1) = cmap [1][(guchar) v];
		*(pixel+2) = cmap [2][(guchar) v];
		*(pixel+3) = (guchar) ((v == context->gif89.transparent) ? 0 : 255);
	} else {
		pixel = dest + (context->draw_ypos + offset) * gdk_pixbuf_get_rowstride (context->frame->pixbuf) + context->draw_xpos * 3;
		*pixel = cmap [0][(guchar) v];
		*(pixel+1) = cmap [1][(guchar) v];
		*(pixel+2) = cmap [2][(guchar) v];
	}
}


/* only called if progressive and interlaced */
static void
gif_fill_in_lines (GifContext *context, guchar *dest, guchar v)
{
	switch (context->draw_pass) {
	case 0:
		if (context->draw_ypos > 4) {
			gif_fill_in_pixels (context, dest, -4, v);
			gif_fill_in_pixels (context, dest, -3, v);
		}
		if (context->draw_ypos < (context->frame_height - 4)) {
			gif_fill_in_pixels (context, dest, 3, v);
			gif_fill_in_pixels (context, dest, 4, v);
		}
		/* we don't need a break here.  We draw the outer pixels first, then the
		 * inner ones, then the innermost ones.  case 0 needs to draw all 3 bands.
		 * case 1, just the last two, and case 2 just draws the last one*/
	case 1:
		if (context->draw_ypos > 2)
			gif_fill_in_pixels (context, dest, -2, v);
		if (context->draw_ypos < (context->frame_height - 2))
			gif_fill_in_pixels (context, dest, 2, v);
		/* no break as above. */
	case 2:
		if (context->draw_ypos > 1)
			gif_fill_in_pixels (context, dest, -1, v);
		if (context->draw_ypos < (context->frame_height - 1))
			gif_fill_in_pixels (context, dest, 1, v);
	case 3:
	default:
		break;
	}
}

/* Clips a rectancle to the base dimensions. Returns TRUE if the clipped rectangle is non-empty. */
static gboolean
clip_frame (GifContext *context,
            gint       *x,
            gint       *y,
            gint       *width,
            gint       *height)
{
        gint orig_x, orig_y;

        orig_x = *x;
        orig_y = *y;
	*x = MAX (0, *x);
	*y = MAX (0, *y);
	*width = MIN (context->width, orig_x + *width) - *x;
	*height = MIN (context->height, orig_y + *height) - *y;

	if (*width > 0 && *height > 0)
		return TRUE;

	/* The frame is completely off-bounds */

	*x = 0;
	*y = 0;
	*width = 0;
	*height = 0;

        return FALSE;
}

/* Call update_func on the given rectangle, unless it is completely off-bounds */
static void
maybe_update (GifContext *context,
              gint        x,
              gint        y,
              gint        width,
              gint        height)
{
        if (clip_frame (context, &x, &y, &width, &height))
                (*context->update_func) (context->frame->pixbuf,
                                         x, y, width, height,
                                         context->user_data);
}

static GifResult
gif_get_lzw (GifContext *context)
{
	guchar *dest, *temp;
	gint lower_bound, upper_bound; /* bounds for emitting the area_updated signal */
	gboolean bound_flag;
	gint first_pass; /* bounds for emitting the area_updated signal */
	gint v;
	GifResult result;

	if (context->frame == NULL) {
                context->frame = g_new (GdkPixbufFrame, 1);

                context->frame->composited = NULL;
                context->frame->revert = NULL;

                if (context->frame_len == 0 || context->frame_height == 0) {
                        /* An empty frame, we just output a single transparent
                         * pixel at (0, 0).
                         */
                        context->x_offset = 0;
                        context->y_offset = 0;
                        context->frame_len = 1;
                        context->frame_height = 1;
                        context->frame->pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, 1, 1);
                        if (context->frame->pixbuf) {
                                guchar *pixels;

                                pixels = gdk_pixbuf_get_pixels (context->frame->pixbuf);
                                pixels[0] = 0;
                                pixels[1] = 0;
                                pixels[2] = 0;
                                pixels[3] = 0;
                        }
                } else
                        context->frame->pixbuf =
                                gdk_pixbuf_new (GDK_COLORSPACE_RGB,
                                                TRUE,
                                                8,
                                                context->frame_len,
                                                context->frame_height);
                if (!context->frame->pixbuf) {
                        g_free (context->frame);
                        g_set_error_literal (context->error,
                                             GDK_PIXBUF_ERROR,
                                             GDK_PIXBUF_ERROR_INSUFFICIENT_MEMORY,
                                             _("Not enough memory to load GIF file"));
                        GifResult ret = {.type = GIF_RESULT_FAILURE};
                        return ret;
                }

                context->frame->x_offset = context->x_offset;
                context->frame->y_offset = context->y_offset;
                context->frame->need_recomposite = TRUE;

                /* GIF delay is in hundredths, we want thousandths */
                context->frame->delay_time = context->gif89.delay_time * 10;

                /* GIFs with delay time 0 are mostly broken, but they
                 * just want a default, "not that fast" delay.
                 */
                if (context->frame->delay_time == 0)
                        context->frame->delay_time = 100;

                /* No GIFs gets to play faster than 50 fps. They just
                 * lock up poor gtk.
                 */
                if (context->frame->delay_time < 20)
                        context->frame->delay_time = 20; /* 20 = "fast" */

                context->frame->elapsed = context->animation->total_time;
                context->animation->total_time += context->frame->delay_time;

                switch (context->gif89.disposal) {
                case 0:
                case 1:
                        context->frame->action = GDK_PIXBUF_FRAME_RETAIN;
                        break;
                case 2:
                        context->frame->action = GDK_PIXBUF_FRAME_DISPOSE;
                        break;
                case 3:
                        context->frame->action = GDK_PIXBUF_FRAME_REVERT;
                        break;
                default:
                        context->frame->action = GDK_PIXBUF_FRAME_RETAIN;
                        break;
                }

                context->frame->bg_transparent = (context->gif89.transparent == context->background_index);

                context->animation->n_frames ++;
                context->animation->frames = g_list_append (context->animation->frames, context->frame);

                /* Only call prepare_func for the first frame */
		if (context->animation->frames->next == NULL) {
                        if (context->animation->width == 0 )
                                context->animation->width = gdk_pixbuf_get_width(context->frame->pixbuf);
                        if (context->animation->height == 0)
                                context->animation->height = gdk_pixbuf_get_height (context->frame->pixbuf);

                        if (context->prepare_func)
                                (* context->prepare_func) (context->frame->pixbuf,
                                                           GDK_PIXBUF_ANIMATION (context->animation),
                                                           context->user_data);
                } else {
                        /* Otherwise init frame with last frame */
                        GList *link;
                        GdkPixbufFrame *prev_frame;
                        gint x, y, w, h;

                        link = g_list_find (context->animation->frames, context->frame);

                        prev_frame = link->prev->data;

                        gdk_pixbuf_gif_anim_frame_composite (context->animation, prev_frame);

                        /* Composite failed */
                        if (prev_frame->composited == NULL) {
                                GdkPixbufFrame *frame = NULL;
                                link = g_list_first (context->animation->frames);
                                while (link != NULL) {
                                        frame = (GdkPixbufFrame *)link->data;
                                        if (frame != NULL) {
                                                if (frame->pixbuf != NULL)
                                                        g_object_unref (frame->pixbuf);
                                                if (frame->composited != NULL)
                                                        g_object_unref (frame->composited);
                                                if (frame->revert != NULL)
                                                        g_object_unref (frame->revert);
                                                g_free (frame);
                                        }
                                        link = link->next;
                                }

                                g_list_free (context->animation->frames);
                                context->animation->frames = NULL;

                                g_set_error_literal (context->error,
                                                     GDK_PIXBUF_ERROR,
                                                     GDK_PIXBUF_ERROR_INSUFFICIENT_MEMORY,
                                                     _("Not enough memory to composite a frame in GIF file"));
                                GifResult ret = {.type = GIF_RESULT_FAILURE};
                                return ret;
                        }

                        x = context->frame->x_offset;
                        y = context->frame->y_offset;
                        w = gdk_pixbuf_get_width (context->frame->pixbuf);
                        h = gdk_pixbuf_get_height (context->frame->pixbuf);
                        if (clip_frame (context, &x, &y, &w, &h))
                                gdk_pixbuf_copy_area (prev_frame->composited,
                                                      x, y, w, h,
                                                      context->frame->pixbuf,
                                                      0, 0);
                }
        }

	dest = gdk_pixbuf_get_pixels (context->frame->pixbuf);

	bound_flag = FALSE;
	lower_bound = upper_bound = context->draw_ypos;
	first_pass = context->draw_pass;

	while (TRUE) {
                guchar (*cmap)[MAXCOLORMAPSIZE];

                if (context->frame_cmap_active)
                        cmap = context->frame_color_map;
                else
                        cmap = context->global_color_map;

		
		
		GifResult read_result = lzw_read_byte (context);
		printf ("read byte returned us: %d: %d\n", read_result.type, read_result.byte_value);
		if (read_result.type != GIF_RESULT_OKAY_BYTE) {
			result = read_result;
			goto finished_data;
		}
		v = read_result.byte_value;
        result = (GifResult) {.type = GIF_RESULT_OKAY_BYTE, .byte_value = read_result.byte_value};
		bound_flag = TRUE;


        g_assert (gdk_pixbuf_get_has_alpha (context->frame->pixbuf));

		        if (!(G_MINUCHAR <= v && v <= G_MAXUCHAR)) {
			        g_warning ("gif: Expected %d <= %d <= %d\n",  G_MINUCHAR, v, G_MAXUCHAR);
		        }
		        guchar b = (guchar) v;
                temp = dest + context->draw_ypos * gdk_pixbuf_get_rowstride (context->frame->pixbuf) + context->draw_xpos * 4;
                *temp = cmap [0][b];
                *(temp+1) = cmap [1][b];
                *(temp+2) = cmap [2][b];
                *(temp+3) = (b == context->gif89.transparent) ? 0 : 255;

		if (context->prepare_func && context->frame_interlace)
    			gif_fill_in_lines (context, dest, b);

		context->draw_xpos++;

		if (context->draw_xpos == context->frame_len) {
			context->draw_xpos = 0;
			if (context->frame_interlace) {
				switch (context->draw_pass) {
				case 0:
				case 1:
					context->draw_ypos += 8;
					break;
				case 2:
					context->draw_ypos += 4;
					break;
				case 3:
					context->draw_ypos += 2;
					break;
				}

				if (context->draw_ypos >= context->frame_height) {
					context->draw_pass++;
					switch (context->draw_pass) {
					case 1:
						context->draw_ypos = 4;
						break;
					case 2:
						context->draw_ypos = 2;
						break;
					case 3:
						context->draw_ypos = 1;
						break;
					default:
						goto done;
					}
				}
			} else {
				context->draw_ypos++;
			}
			if (context->draw_pass != first_pass) {
				if (context->draw_ypos > lower_bound) {
					lower_bound = 0;
					upper_bound = context->frame_height;
				} else {

				}
			} else
				upper_bound = context->draw_ypos;
		}
		if (context->draw_ypos >= context->frame_height)
			break;
	}

 done:

        context->state = GIF_GET_NEXT_STEP; 

        result = (GifResult) {.type = GIF_RESULT_OKAY_BYTE, .byte_value = 0};

 finished_data:

        if (bound_flag)
                context->frame->need_recomposite = TRUE;

	if (bound_flag && context->update_func) {
		if (lower_bound <= upper_bound && first_pass == context->draw_pass) {
                        maybe_update (context,
                                      context->frame->x_offset,
                                      context->frame->y_offset + lower_bound,
                                      gdk_pixbuf_get_width (context->frame->pixbuf),
                                      upper_bound - lower_bound + 1);
		} else {
			if (lower_bound <= upper_bound) {
                                maybe_update (context,
                                              context->frame->x_offset,
                                              context->frame->y_offset,
                                              gdk_pixbuf_get_width (context->frame->pixbuf),
                                              gdk_pixbuf_get_height (context->frame->pixbuf));
			} else {
                                maybe_update (context,
                                              context->frame->x_offset,
                                              context->frame->y_offset,
                                              gdk_pixbuf_get_width (context->frame->pixbuf),
                                              upper_bound);
                                maybe_update (context,
                                              context->frame->x_offset,
                                              context->frame->y_offset + lower_bound,
                                              gdk_pixbuf_get_width (context->frame->pixbuf),
                                              gdk_pixbuf_get_height (context->frame->pixbuf) - lower_bound);
			}
		}
	}

	if (context->state == GIF_GET_NEXT_STEP) {
                /* Will be freed with context->animation, we are just
                 * marking that we're done with it (no current frame)
                 */
		context->frame = NULL;
                context->frame_cmap_active = FALSE;

                if (context->stop_after_first_frame)
                        context->state =  GIF_DONE;
	}

	//GifResult res = {.type = GIF_RESULT_OKAY_BYTE, .byte_value = v};
        return result;
}

static void
gif_set_prepare_lzw (GifContext *context)
{
	context->state = GIF_PREPARE_LZW;
	context->lzw_code_pending = -1;
}
static GifResult
gif_prepare_lzw (GifContext *context)
{
	gint i;

	if (!gif_read (context, &(context->lzw_set_code_size), 1)) {
		/*g_message (_("GIF: EOF / read error on image data\n"));*/
		return (GifResult) {.type = GIF_RESULT_MORE_BYTES};
	}

        if (context->lzw_set_code_size > MAX_LZW_BITS) {
                g_set_error_literal (context->error,
                                     GDK_PIXBUF_ERROR,
                                     GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                                     _("GIF image is corrupt (incorrect LZW compression)"));
                        GifResult ret = {.type = GIF_RESULT_FAILURE};
                        return ret;
        }

	context->lzw_code_size = context->lzw_set_code_size + 1;
	context->lzw_clear_code = 1 << context->lzw_set_code_size;
	context->lzw_end_code = context->lzw_clear_code + 1;
	printf ("end code: %d\n", context->lzw_end_code);
	context->lzw_max_code_size = 2 * context->lzw_clear_code;
	context->lzw_max_code = context->lzw_clear_code + 2;
	context->lzw_fresh = TRUE;
	context->code_curbit = 0;
	context->code_lastbit = 0;
	context->code_last_byte = 0;
	context->code_done = FALSE;

        g_assert (context->lzw_clear_code <=
                  G_N_ELEMENTS (context->lzw_table[0]));

	for (i = 0; i < context->lzw_clear_code; ++i) {
		context->lzw_table[0][i] = 0;
		context->lzw_table[1][i] = i;
	}
	for (; i < (1 << MAX_LZW_BITS); ++i)
		context->lzw_table[0][i] = context->lzw_table[1][0] = 0;

	context->lzw_sp = context->lzw_stack;
	gif_set_get_lzw (context);

	return (GifResult) {.type = GIF_RESULT_OKAY};
}

/* needs 13 bytes to proceed. */
static GifResult
gif_init (GifContext *context)
{
	unsigned char buf[16];
	char version[4];
	gint width, height;

	if (!gif_read (context, buf, 6)) {
		/* Unable to read magic number,
                 * gif_read() should have set error
                 */
                return (GifResult) {.type = GIF_RESULT_MORE_BYTES};
	}

	if (strncmp ((char *) buf, "GIF", 3) != 0) {
		/* Not a GIF file */
                g_set_error_literal (context->error,
                                     GDK_PIXBUF_ERROR,
                                     GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                                     _("File does not appear to be a GIF file"));
                        return (GifResult) {.type = GIF_RESULT_FAILURE};
	}

	strncpy (version, (char *) buf + 3, 3);
	version[3] = '\0';

	if ((strcmp (version, "87a") != 0) && (strcmp (version, "89a") != 0)) {
		/* bad version number, not '87a' or '89a' */
                g_set_error (context->error,
                             GDK_PIXBUF_ERROR,
                             GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                             _("Version %s of the GIF file format is not supported"),
                             version);
                        return (GifResult) {.type = GIF_RESULT_FAILURE};
	}

	/* read the screen descriptor */
	if (!gif_read (context, buf, 7)) {
		/* Failed to read screen descriptor, error set */
		return (GifResult) {.type = GIF_RESULT_MORE_BYTES};
	}

	context->width = LM_to_uint (buf[0], buf[1]);
	context->height = LM_to_uint (buf[2], buf[3]);
        /* The 4th byte is
         * high bit: whether to use the background index
         * next 3:   color resolution
         * next:     whether colormap is sorted by priority of allocation
         * last 3:   size of colormap
         */
	context->global_bit_pixel = 2U << (buf[4] & 0x07);
	context->global_color_resolution = (((buf[4] & 0x70) >> 3) + 1);
        context->has_global_cmap = (buf[4] & 0x80) != 0;
	context->background_index = buf[5];
	context->aspect_ratio = buf[6];

        /* Use background of transparent black as default, though if
         * one isn't set explicitly no one should ever use it.
         */
        context->animation->bg_red = 0;
        context->animation->bg_green = 0;
        context->animation->bg_blue = 0;

        if (!(context->width <= G_MAXINT)) {
            g_warning ("Expected context->width <= G_MAXINT: %u\n", context->width);
        }
        context->animation->width = width = (int) context->width;

        if (!(context->height <= G_MAXINT)) {
            g_warning ("Expected context->height <= G_MAXINT: %u\n", context->height);
        }
        context->animation->height = height = (int) context->height;

        if (context->size_func) {
                (*context->size_func) (&width, &height, context->user_data);
                if (width == 0 || height == 0) {
                        g_set_error (context->error,
                                             GDK_PIXBUF_ERROR,
                                             GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                                             _("Resulting GIF image has zero size: %dx%d"), height, width);
                        GifResult ret = {.type = GIF_RESULT_FAILURE};
                        return ret;
                }
        }

	if (context->has_global_cmap) {
		gif_set_get_colormap (context);
	} else {
		context->state = GIF_GET_NEXT_STEP;
	}

#ifdef DUMP_IMAGE_DETAILS
        g_print (">Image width: %d height: %d global_cmap: %d background: %d\n",
                 context->width, context->height, context->has_global_cmap, context->background_index);
#endif

	return (GifResult) {.type = GIF_RESULT_OKAY};
}

static void
gif_set_get_frame_info (GifContext *context)
{
	context->state = GIF_GET_FRAME_INFO;
}

static GifResult
gif_get_frame_info (GifContext *context)
{
	unsigned char buf[9];

	if (!gif_read (context, buf, 9))
                return (GifResult) {.type = GIF_RESULT_MORE_BYTES};

	/* Okay, we got all the info we need.  Lets record it */
	context->frame_len = LM_to_uint (buf[4], buf[5]);
	context->frame_height = LM_to_uint (buf[6], buf[7]);
	context->x_offset = LM_to_uint (buf[0], buf[1]);
	context->y_offset = LM_to_uint (buf[2], buf[3]);

	if (context->animation->frames == NULL &&
            context->gif89.disposal == 3) {
                /* First frame can't have "revert to previous" as its
                 * dispose mode. Silently use "retain" instead.
                 */
                context->gif89.disposal = 0;
	}

	context->frame_interlace = BitSet (buf[8], INTERLACE);

#ifdef DUMP_IMAGE_DETAILS
        g_print (">width: %d height: %d xoffset: %d yoffset: %d disposal: %d delay: %d transparent: %d interlace: %d\n",
                 context->frame_len, context->frame_height, context->x_offset, context->y_offset,
                 context->gif89.disposal, context->gif89.delay_time, context->gif89.transparent, context->frame_interlace);
#endif

	if (BitSet (buf[8], LOCALCOLORMAP)) {

#ifdef DUMP_IMAGE_DETAILS
                g_print (">has local colormap\n");
#endif

		/* Does this frame have it's own colormap. */
		/* really only relevant when looking at the first frame
		 * of an animated gif. */
		/* if it does, we need to re-read in the colormap,
		 * the gray_scale, and the bit_pixel */
                context->frame_cmap_active = TRUE;
		context->frame_bit_pixel = 1U << ((buf[8] & 0x07) + 1);
		gif_set_get_colormap2 (context);
		return (GifResult) {.type = GIF_RESULT_OKAY};
	}

        if (!context->has_global_cmap) {
                context->state = GIF_DONE;

                g_set_error_literal (context->error,
                                     GDK_PIXBUF_ERROR,
                                     GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                                     _("GIF image has no global colormap, and a frame inside it has no local colormap."));

                        GifResult ret = {.type = GIF_RESULT_FAILURE};
                        return ret;
        }

	gif_set_prepare_lzw (context);
	return (GifResult) {.type = GIF_RESULT_OKAY};

}

static GifResult
gif_get_next_step (GifContext *context)
{
	unsigned char c;
	while (TRUE) {
		if (!gif_read (context, &c, 1)) {
			return (GifResult) {.type = GIF_RESULT_MORE_BYTES};
		}
		if (c == ';') {
			/* GIF terminator */
			/* hmm.  Not 100% sure what to do about this.  Should
			 * i try to return a blank image instead? */
			context->state = GIF_DONE;
			return (GifResult) {.type = GIF_RESULT_OKAY};
		}

		if (c == '!') {
			/* Check the extension */
			gif_set_get_extension (context);
			return (GifResult) {.type = GIF_RESULT_OKAY};
		}

		/* look for frame */
		if (c != ',') {
			/* Not a valid start character */
			continue;
		}
		/* load the frame */
		gif_set_get_frame_info (context);
		return (GifResult) {.type = GIF_RESULT_OKAY};
	}
}


#define LOG(x) g_print ("%s: %s\n", ""/*G_STRLOC*/, x);




static GifResult
gif_main_loop (GifContext *context)
{
	GifResult retval = {.type = GIF_RESULT_OKAY};

	do {
		switch (context->state) {
		case GIF_START:
                        LOG("start\n");
			retval = gif_init (context);
			break;

		case GIF_GET_COLORMAP:
                        LOG("get_colormap\n");
			retval = gif_get_colormap (context);
			if (retval.type == GIF_RESULT_OKAY)
				context->state = GIF_GET_NEXT_STEP;
			break;

		case GIF_GET_NEXT_STEP:
                        LOG("next_step\n");
			retval = gif_get_next_step (context);
			break;

		case GIF_GET_FRAME_INFO:
                        LOG("frame_info\n");
			retval = gif_get_frame_info (context);
			break;

		case GIF_GET_EXTENSION:
                        LOG("get_extension\n");
			retval = gif_get_extension (context);
			if (retval.type == GIF_RESULT_OKAY)
				context->state = GIF_GET_NEXT_STEP;
			break;

		case GIF_GET_COLORMAP2:
                        LOG("get_colormap2\n");
			retval = gif_get_colormap2 (context);
			if (retval.type == GIF_RESULT_OKAY)
				gif_set_prepare_lzw (context);
			break;

		case GIF_PREPARE_LZW:
                        LOG("prepare_lzw\n");
			retval = gif_prepare_lzw (context);
			printf ("PREP_LZW: t: %d  v: %d\n",  retval.type, retval.byte_value);
			break;

		case GIF_LZW_FILL_BUFFER:
                        LOG("fill_buffer\n");
			retval = gif_lzw_fill_buffer (context);
			break;

		case GIF_LZW_CLEAR_CODE:
                        LOG("clear_code\n");
			retval = gif_lzw_clear_code (context);
			break;

		case GIF_GET_LZW:
                        LOG("get_lzw\n");
			retval = gif_get_lzw (context);
			printf ("GET_LZW: t: %d  v: %d\n", retval.type, retval.byte_value);
			break;

		case GIF_DONE:
                        LOG("done\n");
                        /* fall through */
		default:
		    // We do hit this case!!1
		    // g_assert_not_reached ();
			retval = (GifResult) {.type = GIF_RESULT_OKAY};
			// Hrm. Or do we return a byte with "0"..?
			goto done;
		};
	} while ((retval.type == GIF_RESULT_OKAY) || (retval.type == GIF_RESULT_OKAY_BYTE  &&  retval.byte_value == 0) || (retval.type == GIF_RESULT_TO_MAINLOOP));
 done:
	return retval;
}

static GifContext *
new_context (void)
{
	GifContext *context;

	context = g_try_malloc (sizeof (GifContext));
        if (context == NULL)
                return NULL;

        memset (context, 0, sizeof (GifContext));

        context->animation = g_object_new (GDK_TYPE_PIXBUF_GIF_ANIM, NULL);
	context->frame = NULL;
	context->file = NULL;
	context->state = GIF_START;
	context->size_func = NULL;
	context->prepare_func = NULL;
	context->update_func = NULL;
	context->user_data = NULL;
	context->buf = NULL;
	context->amount_needed = 13;
	context->buf = g_new (guchar, context->amount_needed);
	context->gif89.transparent = -1;
	context->gif89.delay_time = -1;
	context->gif89.input_flag = -1;
	context->gif89.disposal = -1;
        context->animation->loop = 1;
        context->in_loop_extension = FALSE;
        context->stop_after_first_frame = FALSE;

	return context;
}
/* Shared library entry point */
static GdkPixbuf *
gdk_pixbuf__gif_image_load (FILE *file, GError **error)
{
	GifContext *context;
	GdkPixbuf *pixbuf;
        GifResult retval;

	g_return_val_if_fail (file != NULL, NULL);

	context = new_context ();

        if (context == NULL) {
                g_set_error_literal (error,
                                     GDK_PIXBUF_ERROR,
                                     GDK_PIXBUF_ERROR_INSUFFICIENT_MEMORY,
                                     _("Not enough memory to load GIF file"));
                return NULL;
        }

	context->file = file;
        context->error = error;
        context->stop_after_first_frame = TRUE;

        retval = gif_main_loop (context);
	if (retval.type == GIF_RESULT_MORE_BYTES || context->animation->frames == NULL) {
                if (context->error && *(context->error) == NULL)
                        g_set_error_literal (context->error,
                                             GDK_PIXBUF_ERROR,
                                             GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                                             _("GIF file was missing some data (perhaps it was truncated somehow?)"));
        }
        else if (retval.type == GIF_RESULT_FAILURE) {
                pixbuf = NULL;
                goto out;
        }

        pixbuf = gdk_pixbuf_animation_get_static_image (GDK_PIXBUF_ANIMATION (context->animation));

        if (pixbuf)
                g_object_ref (pixbuf);

out:
        g_object_unref (context->animation);

        g_free (context->buf);
	g_free (context);

	return pixbuf;
}

static gpointer
gdk_pixbuf__gif_image_begin_load (GdkPixbufModuleSizeFunc size_func,
                                  GdkPixbufModulePreparedFunc prepare_func,
				  GdkPixbufModuleUpdatedFunc update_func,
				  gpointer user_data,
                                  GError **error)
{
	GifContext *context;

#ifdef IO_GIFDEBUG
	count = 0;
#endif
	context = new_context ();

        if (context == NULL) {
                g_set_error_literal (error,
                                     GDK_PIXBUF_ERROR,
                                     GDK_PIXBUF_ERROR_INSUFFICIENT_MEMORY,
                                     _("Not enough memory to load GIF file"));
                return NULL;
        }

        context->error = error;
	context->size_func = size_func;
	context->prepare_func = prepare_func;
	context->update_func = update_func;
	context->user_data = user_data;

	return (gpointer) context;
}

static gboolean
gdk_pixbuf__gif_image_stop_load (gpointer data, GError **error)
{
	GifContext *context = (GifContext *) data;
        gboolean retval = TRUE;

        if (context->animation->frames == NULL) {
                g_set_error_literal (error,
                                     GDK_PIXBUF_ERROR,
                                     GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                                     _("GIF image was truncated or incomplete."));

                retval = FALSE;
        } else if (context->state != GIF_DONE) {
                g_set_error_literal (error,
                                     GDK_PIXBUF_ERROR,
                                     GDK_PIXBUF_ERROR_INCOMPLETE_ANIMATION,
                                     _("Not all frames of the GIF image were loaded."));

                retval = FALSE;
        }

        g_object_unref (context->animation);

  	g_free (context->buf);
	g_free (context);

        return retval;
}

static gboolean
gdk_pixbuf__gif_image_load_increment (gpointer data,
                                      const guchar *buf, guint size,
                                      GError **error)
{
	GifResult retval;
	GifContext *context = (GifContext *) data;

        context->error = error;

	if (context->amount_needed == 0) {
		/* we aren't looking for some bytes. */
		/* we can use buf now, but we don't want to keep it around at all.
		 * it will be gone by the end of the call. */
		context->buf = (guchar*) buf; /* very dubious const cast */
		context->ptr = 0;
		context->size = size;
	} else {
		/* we need some bytes */
		if (size < context->amount_needed) {
			context->amount_needed -= size;
			/* copy it over and return */
			memcpy (context->buf + context->size, buf, size);
			context->size += size;
			return TRUE;
		} else if (size == context->amount_needed) {
			memcpy (context->buf + context->size, buf, size);
			context->size += size;
		} else {
			context->buf = g_realloc (context->buf, context->size + size);
			memcpy (context->buf + context->size, buf, size);
			context->size += size;
		}
	}

	retval = gif_main_loop (context);

	if (retval.type == GIF_RESULT_FAILURE) {
		if (context->buf == buf)
                        context->buf = NULL;
		return FALSE;
        }
	if (retval.type == GIF_RESULT_MORE_BYTES) {
		/* we didn't have enough memory */
		/* prepare for the next image_load_increment */
		if (context->buf == buf) {
			g_assert (context->size == size);
			context->buf = g_new (guchar, context->amount_needed + (context->size - context->ptr));
			memcpy (context->buf, buf + context->ptr, context->size - context->ptr);
		} else {
			/* copy the left overs to the begining of the buffer */
			/* and realloc the memory */
			memmove (context->buf, context->buf + context->ptr, context->size - context->ptr);
			context->buf = g_realloc (context->buf, context->amount_needed + (context->size - context->ptr));
		}
		context->size = context->size - context->ptr;
		context->ptr = 0;
	} else {
		/* we are prolly all done */
		if (context->buf == buf)
			context->buf = NULL;
	}
	return TRUE;
}

static GdkPixbufAnimation *
gdk_pixbuf__gif_image_load_animation (FILE *file,
                                      GError **error)
{
	GifContext *context;
	GdkPixbufAnimation *animation;

	g_return_val_if_fail (file != NULL, NULL);

	context = new_context ();

        if (context == NULL) {
                g_set_error_literal (error,
                                     GDK_PIXBUF_ERROR,
                                     GDK_PIXBUF_ERROR_INSUFFICIENT_MEMORY,
                                     _("Not enough memory to load GIF file"));
                return NULL;
        }

        context->error = error;
	context->file = file;

	if ((gif_main_loop (context).type == GIF_RESULT_MORE_BYTES) || context->animation->frames == NULL) {
                if (context->error && *(context->error) == NULL)
                        g_set_error_literal (context->error,
                                             GDK_PIXBUF_ERROR,
                                             GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
                                             _("GIF file was missing some data (perhaps it was truncated somehow?)"));

                g_object_unref (context->animation);
                context->animation = NULL;
        }

        if (context->animation)
                animation = GDK_PIXBUF_ANIMATION (context->animation);
        else
                animation = NULL;

        if (context->error && *(context->error))
                g_print ("%s\n", (*(context->error))->message);

        g_free (context->buf);
	g_free (context);
	return animation;
}

#ifndef INCLUDE_gif
#define MODULE_ENTRY(function) G_MODULE_EXPORT void function
#else
#define MODULE_ENTRY(function) void _gdk_pixbuf__gif_ ## function
#endif

MODULE_ENTRY (fill_vtable) (GdkPixbufModule *module)
{
        module->load = gdk_pixbuf__gif_image_load;
        module->begin_load = gdk_pixbuf__gif_image_begin_load;
        module->stop_load = gdk_pixbuf__gif_image_stop_load;
        module->load_increment = gdk_pixbuf__gif_image_load_increment;
        module->load_animation = gdk_pixbuf__gif_image_load_animation;
}

MODULE_ENTRY (fill_info) (GdkPixbufFormat *info)
{
        static const GdkPixbufModulePattern signature[] = {
                { "GIF8", NULL, 100 },
                { NULL, NULL, 0 }
        };
	static const gchar *mime_types[] = {
		"image/gif",
		NULL
	};
	static const gchar *extensions[] = {
		"gif",
		NULL
	};

	info->name = "gif";
        info->signature = (GdkPixbufModulePattern *) signature;
	info->description = NC_("image format", "GIF");
	info->mime_types = (gchar **) mime_types;
	info->extensions = (gchar **) extensions;
	info->flags = GDK_PIXBUF_FORMAT_THREADSAFE;
	info->license = "LGPL";
}
