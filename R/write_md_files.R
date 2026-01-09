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

        func_name <- tools::file_path_sans_ext(basename(file))
        func_name_lower <- tolower(func_name)

        # Warn if function name contains capitals (Astro requires lowercase)
        if (func_name != func_name_lower) {
          cli::cli_warn("Function {.fn {func_name}} contains capitals - MDX filename will be lowercased to {.file {func_name_lower}.mdx}")
        }

        out_path <- file.path(output_dir, paste0(func_name_lower, file_ext))

        # Append example outputs if they exist
        if (!is.null(site_output_path)) {
          md_content <- append_example_outputs(md_content, func_name, site_output_path)
        }

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

#' Append example outputs to markdown content using MDX imports
#'
#' Checks for png and txt example output files and appends MDX import
#' statements after the Examples section.
#'
#' @param md_content Markdown content string
#' @param func_name Function name to look for outputs
#' @param site_output_path Path to site output directory
#' @return Updated markdown content with example outputs appended
#' @keywords internal
append_example_outputs <- function(md_content, func_name, site_output_path) {
  # Check for example output files
  png_path <- file.path(site_output_path, "public", "examples", paste0(func_name, ".png"))
  txt_path <- file.path(site_output_path, "public", "examples", "text", paste0(func_name, ".txt"))
  html_path <- file.path(site_output_path, "public", "examples", paste0(func_name, ".html"))

  has_png <- file.exists(png_path)
  has_txt <- file.exists(txt_path)
  has_html <- file.exists(html_path)

  if (!has_png && !has_txt && !has_html) {
    return(md_content)
  }

  # Build the import statements and JSX components
  imports <- character()
  components <- character()

  if (has_txt) {
    imports <- c(imports, sprintf("import exampleOutput from '/examples/text/%s.txt?raw';", func_name))
    components <- c(components, '<pre style="overflow-x: auto;">{exampleOutput}</pre>')
  }

  if (has_png) {
    components <- c(components, sprintf('<img src={`${import.meta.env.BASE_URL}examples/%s.png`} alt="Example plot" style="max-width: 100%%;" />', func_name))
  }

  if (has_html) {
    # Use iframe for gt tables / HTML output
    components <- c(components, sprintf('<iframe
  src={`${import.meta.env.BASE_URL}examples/%s.html`}
  style="width: 100%%; border: none;"
  onload="this.style.height=(this.contentWindow.document.body.scrollHeight + 50)+\'px\';"
></iframe>', func_name))
  }

  # Build the output block
  output_block <- paste0(
    "\n### Output\n\n",
    if (length(imports) > 0) paste0(paste(imports, collapse = "\n"), "\n\n") else "",
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
        insert_pos <- examples_start[1] + closing_backticks[length(closing_backticks)] - 1

        lines <- c(
          lines[1:insert_pos],
          strsplit(output_block, "\n")[[1]],
          if (insert_pos < length(lines)) lines[(insert_pos + 1):length(lines)] else character()
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
      if (length(imports) > 0) paste0(paste(imports, collapse = "\n"), "\n\n") else "",
      paste(components, collapse = "\n\n")
    )
  }

  md_content
}
