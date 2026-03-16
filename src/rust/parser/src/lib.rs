use extendr_api::prelude::*;

pub mod parsing;
pub mod document;
pub mod emit;

pub use document::Document;

// Generate extendr module for R integration
extendr_module! {
    mod starlightr_parser;
}
