use extendr_api::Result;
use extendr_api::prelude::*;

use fs_err as fs;
use std::path::{Path, PathBuf};

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

pub trait OptionExt<T> {
    fn ok_or_extendr_err(self, message: impl Into<String>) -> Result<T>;
}

impl<T> OptionExt<T> for Option<T> {
    fn ok_or_extendr_err(self, message: impl Into<String>) -> Result<T> {
        self.ok_or_else(|| Error::Other(message.into()))
    }
}

#[macro_export]
macro_rules! extendr_err {
    ($($arg:tt)*) => {
        Error::Other(format!($($arg)*))
    };
}

fn resolve_output_file_path<R, O>(rd_file: R, output_dir: O) -> Result<PathBuf>
where
    R: AsRef<Path>,
    O: AsRef<Path>,
{
    let rd_file = rd_file.as_ref();
    let output_dir = output_dir.as_ref();

    let file_stem = rd_file
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_extendr_err("Cannot determine Rd file stem")?;

    let slug = file_stem.to_lowercase().replace('.', "-");

    let output_dir = if output_dir.is_absolute() {
        output_dir.to_path_buf()
    } else {
        std::env::current_dir()
            .map_to_extendr_err("Cannot determine current working directory")?
            .join(output_dir)
    };

    if output_dir.exists() {
        if !output_dir.is_dir() {
            return Err(extendr_err!(
                "`output_dir` is not a directory: {}",
                output_dir.display()
            ));
        }
    } else {
        fs::create_dir_all(&output_dir).map_to_extendr_err(format!(
            "Failed to create output directory: {}",
            output_dir.display()
        ))?;
    }

    Ok(output_dir.join(format!("{slug}.mdx")))
}

fn render_reference_path<R, O>(
    rd_file: R,
    output_dir: O,
    emit_options: &EmitOptions,
) -> Result<()> 
where
    R: AsRef<Path>,
    O: AsRef<Path>,
{
    let rd_file = rd_file.as_ref();
    let output_dir = output_dir.as_ref();

    let document = parse_file(rd_file)
        .map_to_extendr_err(format!("Failed to parse rd file: {}", rd_file.display()))?;

    let mdx_contents =
        emit_document(document, emit_options).map_to_extendr_err("Failed to create mdx file")?;

    let output_file = resolve_output_file_path(rd_file, output_dir)?;
    
    fs::write(&output_file, mdx_contents).map_to_extendr_err(format!(
        "Failed to write parsed Rd file to mdx file: {output_file:?}"
    ))
}

/// render_reference
///
/// Renders an .Rd file to an mdx file.
///
/// @param rd_file path to Rd file to convert to .mdx
/// @param output_dir path to directory where reference mdx files are saved
/// @param config_file path to _starlightr.toml
///
/// @return NULL
/// @export
///
/// @examples \dontrun{
/// render_reference(
///   rd_file = "man/function.Rd",
///   output_dir = "../Docs/pkg-docs/src/content/docs/reference",
///   config_file = "_starlightr.toml"
/// )
/// }
#[extendr]
pub fn render_reference(
    rd_file: &str,
    output_dir: &str,
    #[extendr(default = "'_starlightr.toml'")] config_file: &str,
) -> Result<()> {
    let rd_file = Path::new(rd_file)
        .canonicalize()
        .map_to_extendr_err("Failed to canonicalize Rd file path")?;

    let config_file = Path::new(config_file)
        .canonicalize()
        .map_to_extendr_err("Failed to canonicalize config file path")?;

    let output_dir = Path::new(output_dir);

    let emit_options = EmitOptions::from_file(&config_file)
        .map_to_extendr_err(format!("Failed to read config file: {config_file:?}"))?;

    render_reference_path(rd_file, output_dir, &emit_options)
}

#[extendr]
pub fn render_references(
    rd_dir: &str,
    output_dir: &str,
    #[extendr(default = "'_starlightr.toml'")] config_file: &str,
) -> Result<()> {
    let rd_dir = Path::new(rd_dir)
        .canonicalize()
        .map_to_extendr_err("Failed to canonicalize Rd directory path")?;

    if !rd_dir.is_dir() {
        return Err(extendr_err!(
            "`rd_dir` is not a directory: {}",
            rd_dir.display()
        ));
    }

    let config_file = Path::new(config_file)
        .canonicalize()
        .map_to_extendr_err("Failed to canonicalize config file path")?;
    
    let emit_options = EmitOptions::from_file(&config_file)
        .map_to_extendr_err(format!("Failed to read config file: {config_file:?}"))?;

    let output_dir = Path::new(output_dir);

    for entry in fs::read_dir(&rd_dir)
        .map_to_extendr_err(format!("Failed to read `rd_dir`: {}", rd_dir.display()))?
    {
        let entry = entry.map_to_extendr_err("Failed to get entry")?;
        let path = entry.path();

        if path.extension().and_then(|ext| ext.to_str()) == Some("Rd") {
            render_reference_path(&path, output_dir, &emit_options)?;
        }
    }
    Ok(())
}

// Generate extendr module for R integration
extendr_module! {
    mod starlightr_parser;

    fn render_reference;
    fn render_references;
}
