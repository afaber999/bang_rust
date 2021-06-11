
/// This is an implementation detail and *should not* be called directly!
#[doc(hidden)]
pub fn error_abort__(msg: &str) -> ! {
    use std::io::Write;
    let _ = writeln!(std::io::stderr(), "{}", msg);
    std::process::exit(-1);
}

#[macro_export]
macro_rules! user_error {
    ($($args:tt)*) => {
        match format!($($args)*) {
            msg => $crate::errors::error_abort__(&msg)
        }
    };
}

#[macro_export]
macro_rules! internal_error {
    ($($args:tt)*) => {
        match format!($($args)*) {
            msg => $crate::errors::error_abort__(&msg)
        }
    };
}

