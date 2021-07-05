//use crate::location::Location;

pub enum Error {
    //ExpectedTokenReachedEos(Location<'a>),
    InternalError(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            //Error::ExpectedTokenReachedEos(_) => { write!(f, "ERROR ") },
            Error::InternalError(_) => todo!(),
        }
    }
}

/// This is an implementation detail and *should not* be called directly!
#[doc(hidden)]
pub fn error_abort__(msg: &str) -> ! {
    use std::io::Write;
    std::mem::drop( writeln!(std::io::stderr(), "{}", msg));
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
