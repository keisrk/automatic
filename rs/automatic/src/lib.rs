//!
//!
#![feature(box_patterns, box_syntax)]

extern crate itertools;
extern crate log;
extern crate ron;
extern crate serde;
#[macro_use] extern crate serde_derive;

mod utils;
mod presburger;
mod fol;
mod tseitin;
