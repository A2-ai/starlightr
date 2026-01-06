#' Write Rd objects to Markdown files
#'
#' Converts Rd documentation objects to Markdown and writes them to files
#' using the Rd -> HTML -> Markdown conversion pipeline.
#'
#' @param rd_db An Rd database from [tools::Rd_db()].
#' @param output_dir Path to directory to save markdown files.
#' @param file_ext File extension, either ".md" or ".mdx" (default ".mdx").
#' @param config Configuration list for conversion (see [rd_to_markdown()]).
#' @param site_output_path Path to site output directory (for embedding example outputs).
#' @param pkg_name Package name (for link resolution).
#'
#' @return Invisibly returns a character vector of written file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' rd_db <- tools::Rd_db("mypackage")
#' write_md_files(rd_db, "docs/reference")
#'
#' # With configuration
#' write_md_files(
#'   rd_db,
#'   "docs/reference",
#'   config = list(skip_sections = c("author", "source"))
#' )
#' }
write_md_files <- function(
    rd_db,
    output_dir,
    file_ext = ".mdx",
    config = list(),
    site_output_path = NULL,
    pkg_name = NULL) {
  if (!file_ext %in% c(".md", ".mdx")) {
    cli::cli_abort("file_ext must be '.md' or '.mdx', not {.val {file_ext}}")
  }

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_info("Created output directory: {.path {output_dir}}")
  }

  written_files <- character()
  cli::cli_progress_bar("Writing markdown files", total = length(rd_db))

  for (file in names(rd_db)) {
    cli::cli_progress_update()

    tryCatch(
      {
        md_content <- rd_to_markdown(
          rd_obj = rd_db[[file]],
          config = config,
          output_path = site_output_path,
          pkg_name = pkg_name
        )

        out_path <- file.path(
          output_dir,
          paste0(tools::file_path_sans_ext(basename(file)), file_ext)
        )
        writeLines(md_content, con = out_path)
        written_files <- c(written_files, out_path)
      },
      error = function(e) {
        cli::cli_warn("Failed to convert {.file {file}}: {e$message}")
      }
    )
  }

  cli::cli_progress_done()
  cli::cli_alert_success("Wrote {length(written_files)} markdown files to {.path {output_dir}}")
  invisible(written_files)
}
