use std::cell::RefCell;

#[derive(Debug, Clone, Copy)]
pub struct Location<'a> {
    pub row: usize,
    pub col: usize,
    pub filename : &'a str,
}

pub fn fmt_loc(loc: &Location) -> String {
    format!(
        "{}:{}:{}",
        loc.filename,
        loc.row + 1,
        loc.col + 1
    )
}

pub fn fmt_loc_err(loc: &Location) -> String {
    format!(
        "{}:{}:{}: error:",
        loc.filename,
        loc.row + 1,
        loc.col + 1
    )
}

// AF TODO CHECK IF NEEDED
pub fn fmt_loc_err_msg(loc: &Location, msg: &str) -> String {
    format!(
        "{}:{}:{}: error: {}",
        loc.filename,
        loc.row + 1,
        loc.col + 1,
        msg
    )
}
