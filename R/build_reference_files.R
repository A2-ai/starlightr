#' Build reference MDX files from Rd files
#'
#' Renders all `.Rd` files in a directory to MDX using the Rust parser.
#' External links and example outputs are resolved before rendering so that
#' Rust produces final MDX in a single pass.
#'
#' @param rd_dir Path to directory containing `.Rd` files.
#' @param output_dir Path to directory where reference MDX files are saved.
#' @param config_file Path to `_starlightr.toml`.
#' @param site_output_path Path to site output directory (for embedding example outputs).
#'
#' @return Invisibly returns a character vector of written file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' build_reference_files(
#'   rd_dir = "man",
#'   output_dir = "../pkg-docs/src/content/docs/reference",
#'   config_file = "_starlightr.toml",
#'   site_output_path = "../pkg-docs"
#' )
#' }
build_reference_files <- function(
  rd_dir,
  output_dir,
  config_file = "_starlightr.toml",
  site_output_path = NULL
) {
  if (!dir.exists(rd_dir)) {
    cli::cli_abort("Rd directory not found: {.path {rd_dir}}")
  }

  rd_files <- list.files(rd_dir, pattern = "[.]Rd$", full.names = TRUE)
  if (length(rd_files) == 0) {
    cli::cli_warn("No Rd files found in {.path {rd_dir}}")
    return(invisible(character()))
  }

  dep_packages <- extract_dependency_packages()
  external_links_file <- build_external_link_map(dep_packages)

  example_outputs_file <- NULL
  if (!is.null(site_output_path)) {
    example_outputs_file <- build_example_outputs_map(rd_files, site_output_path)
  }

  render_references(
    rd_dir = rd_dir,
    output_dir = output_dir,
    config_file = config_file,
    external_links_file = external_links_file,
    example_outputs_file = example_outputs_file
  )

  written_files <- file.path(
    output_dir,
    paste0(vapply(rd_files, rd_file_to_slug, character(1)), ".mdx")
  )

  cli::cli_alert_success(
    "Wrote {length(written_files)} reference file{?s} to {.path {output_dir}}"
  )
  invisible(written_files)
}

rd_file_to_slug <- function(path) {
  stem <- tools::file_path_sans_ext(basename(path))
  slugify(stem)
}

#' Build example outputs map from Rd files and site output
#'
#' Scans for example output artifacts (txt, png, html) and builds a JSON
#' manifest for Rust to embed in the generated MDX.
#'
#' @param rd_files Character vector of Rd file paths
#' @param site_output_path Path to site output directory
#' @return Path to temporary JSON file, or NULL if no outputs found
#' @keywords internal
build_example_outputs_map <- function(rd_files, site_output_path) {
  outputs <- list()

  for (rd_file in rd_files) {
    func_name <- tools::file_path_sans_ext(basename(rd_file))

    png_path <- file.path(site_output_path, "public", "examples", paste0(func_name, ".png"))
    txt_path <- file.path(site_output_path, "public", "examples", "text", paste0(func_name, ".txt"))
    html_path <- file.path(site_output_path, "public", "examples", paste0(func_name, ".html"))

    entry <- list()
    if (file.exists(txt_path)) {
      txt_content <- paste(readLines(txt_path, warn = FALSE), collapse = "\n")
      if (nchar(txt_content) > 0) {
        entry$txt <- txt_content
      }
    }
    if (file.exists(png_path)) {
      entry$png <- sprintf("/examples/%s.png", func_name)
    }
    if (file.exists(html_path)) {
      entry$html <- sprintf("/examples/%s.html", func_name)
    }

    if (length(entry) > 0) {
      outputs[[func_name]] <- entry
    }
  }

  if (length(outputs) == 0) {
    return(NULL)
  }

  json_path <- tempfile("starlightr-examples-", fileext = ".json")
  writeLines(jsonlite::toJSON(outputs, auto_unbox = TRUE), json_path)
  json_path
}
