use extendr_api::Result;
use extendr_api::prelude::*;

use fs_err as fs;
use std::path::Path;

pub mod document;
pub mod emit;
pub mod parsing;

pub use document::Document;

use emit::{EmitOptions, emit_document};
use parsing::parse_file;

pub trait ResultExt<T> {
    fn map_to_extendr_err(self, message: impl Into<String>) -> Result<T>;
}

impl<T, E: std::fmt::Debug> ResultExt<T> for std::result::Result<T, E> {
    fn map_to_extendr_err(self, message: impl Into<String>) -> Result<T> {
        self.map_err(|x| extendr_api::Error::Other(format!("{}: {x:?}", message.into())))
    }
}

/// render_reference
///
/// Renders an .Rd file to an mdx file.
///
/// @param rd_file path to Rd file to convert to .mdx
/// @param output_file path to new mdx file
/// @param config_file path to _starlightr.toml
///
/// @return NULL
/// @export
///
/// @examples \dontrun{
/// render_reference(
///   rd_file = "man/function.Rd",
///   output_file = "../Docs/src/content/docs/reference/function.mdx",
///   config_file = "_starlightr.toml"
/// )
/// }
#[extendr]
pub fn render_reference(
    rd_file: &str,
    output_file: &str,
    #[extendr(default = "'_starlightr.toml'")] config_file: &str,
) -> Result<()> {
    let rd_file = Path::new(rd_file);
    let config_file = Path::new(config_file);
    let output_file = Path::new(output_file);

    let emit_options = EmitOptions::from_file(config_file)
        .map_to_extendr_err(format!("Failed to read config file: {config_file:?}"))?;

    let document =
        parse_file(rd_file).map_to_extendr_err(format!("Failed to parse rd file: {rd_file:?}"))?;

    let mdx_contents =
        emit_document(document, &emit_options).map_to_extendr_err("Failed to create mdx file")?;

    fs::write(output_file, mdx_contents).map_to_extendr_err(format!(
        "Failed to write parsed Rd file to mdx file: {output_file:?}"
    ))
}

// Generate extendr module for R integration
extendr_module! {
    mod starlightr_parser;

    fn render_reference;
}
