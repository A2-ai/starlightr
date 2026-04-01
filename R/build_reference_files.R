#' Build reference MDX files from Rd files
#'
#' Renders all `.Rd` files in a directory to MDX using the Rust parser and,
#' when example artifacts exist, appends those outputs to the generated pages.
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

  external_links_file <- build_external_link_map()

  render_references(
    rd_dir = rd_dir,
    output_dir = output_dir,
    config_file = config_file,
    external_links_file = external_links_file
  )

  written_files <- file.path(
    output_dir,
    paste0(vapply(rd_files, rd_file_to_slug, character(1)), ".mdx")
  )

  for (i in seq_along(rd_files)) {
    out_path <- written_files[[i]]
    if (!file.exists(out_path)) {
      cli::cli_warn("Expected reference file not found: {.path {out_path}}")
      next
    }

    if (!is.null(site_output_path)) {
      func_name <- tools::file_path_sans_ext(basename(rd_files[[i]]))
      md_content <- paste(readLines(out_path, warn = FALSE), collapse = "\n")
      md_content <- append_example_outputs(
        md_content,
        func_name,
        site_output_path
      )
      writeLines(md_content, con = out_path)
    }
  }

  cli::cli_alert_success(
    "Wrote {length(written_files)} reference file{?s} to {.path {output_dir}}"
  )
  invisible(written_files)
}

rd_file_to_slug <- function(path) {
  stem <- tools::file_path_sans_ext(basename(path))
  slugify(stem)
}

#' Append example outputs to markdown content
#'
#' Checks for png, txt, and html example output files and appends them
#' after the Examples section using standard markdown/HTML (no JSX).
#'
#' @param md_content Markdown content string
#' @param func_name Function name to look for outputs
#' @param site_output_path Path to site output directory
#' @return Updated markdown content with example outputs appended
#' @keywords internal
append_example_outputs <- function(md_content, func_name, site_output_path) {
  # Check for example output files
  png_path <- file.path(
    site_output_path,
    "public",
    "examples",
    paste0(func_name, ".png")
  )
  txt_path <- file.path(
    site_output_path,
    "public",
    "examples",
    "text",
    paste0(func_name, ".txt")
  )
  html_path <- file.path(
    site_output_path,
    "public",
    "examples",
    paste0(func_name, ".html")
  )

  has_png <- file.exists(png_path)
  has_txt <- file.exists(txt_path)
  has_html <- file.exists(html_path)

  if (!has_png && !has_txt && !has_html) {
    return(md_content)
  }

  # Build output components using standard markdown/HTML (no JSX)
  components <- character()

  if (has_txt) {
    txt_content <- paste(readLines(txt_path, warn = FALSE), collapse = "\n")
    if (nchar(txt_content) > 0) {
      components <- c(components, paste0("```\n", txt_content, "\n```"))
    }
  }

  if (has_png) {
    # Standard markdown image
    components <- c(
      components,
      sprintf("![Example plot](/examples/%s.png)", func_name)
    )
  }

  if (has_html) {
    # Standard HTML iframe (no JSX)
    components <- c(
      components,
      sprintf(
        '<iframe src="/examples/%s.html" style="width: 100%%; min-height: 300px; border: none;"></iframe>',
        func_name
      )
    )
  }

  # Build the output block
  output_block <- paste0(
    "\n### Output\n\n",
    paste(components, collapse = "\n\n"),
    "\n"
  )

  # Find the Examples section and insert after the closing ```
  if (grepl("## Examples", md_content)) {
    lines <- strsplit(md_content, "\n")[[1]]
    examples_start <- grep("^## Examples", lines)

    if (length(examples_start) > 0) {
      # Find all closing ``` after Examples section
      # We need to find the last ``` before the next ## section (or end of file)
      next_section <- grep("^## ", lines[(examples_start[1] + 1):length(lines)])
      if (length(next_section) > 0) {
        section_end <- examples_start[1] + next_section[1] - 1
      } else {
        section_end <- length(lines)
      }

      # Find the last ``` within the Examples section
      examples_lines <- lines[(examples_start[1]):section_end]
      closing_backticks <- grep("^```$", examples_lines)

      if (length(closing_backticks) > 0) {
        # Insert after the last closing ```
        insert_pos <- examples_start[1] +
          closing_backticks[length(closing_backticks)] -
          1

        lines <- c(
          lines[1:insert_pos],
          strsplit(output_block, "\n")[[1]],
          if (insert_pos < length(lines))
            lines[(insert_pos + 1):length(lines)] else character()
        )
        md_content <- paste(lines, collapse = "\n")
      } else {
        # No code block found, insert before next section
        lines <- c(
          lines[1:(section_end - 1)],
          strsplit(output_block, "\n")[[1]],
          lines[section_end:length(lines)]
        )
        md_content <- paste(lines, collapse = "\n")
      }
    }
  } else {
    # No Examples section, append at end
    md_content <- paste0(
      md_content,
      "\n\n## Output\n\n",
      paste(components, collapse = "\n\n")
    )
  }

  md_content
}
