#[no_mangle]
pub extern fn hello() {
    println!("hello world");
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
