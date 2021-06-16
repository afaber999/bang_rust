use std::{cell::RefCell};

#[derive(Debug, Clone , Copy)]
pub struct Location {
    pub row     : usize,
    pub col     : usize,
    pub file_idx: usize,
}

#[derive(Debug, Default)]
pub struct FileNameLocations {
    file_names : RefCell< Vec<String>>,
}

impl FileNameLocations {
    pub fn new() -> Self {
        Self {
            file_names : RefCell::new( Vec::new() ),
        }
    }

    pub fn insert(&self, name:String) ->usize {
        self.file_names.borrow_mut().push( name );
        self.file_names.borrow().len() - 1
    }

    pub fn get(&self, idx: usize ) -> String {
        self.file_names.borrow()[idx].clone()
    }
}

pub fn fmt_loc( file_locations: &FileNameLocations, loc: &Location) -> String {
    format!( "{}:{}:{}", 
        file_locations.get(loc.file_idx),
        loc.row + 1,
        loc.col + 1)
}

pub fn fmt_loc_err( file_locations: &FileNameLocations, loc: &Location) -> String {
    format!( "{}:{}:{}: error:", 
        file_locations.get(loc.file_idx),
        loc.row + 1,
        loc.col + 1)
}

// AF TODO CHECK IF NEEDED
pub fn fmt_loc_err_msg( file_locations: &FileNameLocations, loc: &Location, msg: &str) -> String {
    format!( "{}:{}:{}: error: {}", 
        file_locations.get(loc.file_idx),
        loc.row + 1,
        loc.col + 1,
        msg)
}
