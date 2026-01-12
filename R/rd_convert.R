#' @title Rd to Markdown Conversion
#' @description Convert R documentation (Rd) files to Markdown using R's built-in
#'   Rd2HTML converter and pandoc.
#' @name rd_convert
NULL

#' Extract frontmatter data from Rd object
#'
#' @param rd_obj Rd object
#'
#' @return List with title, description, and aliases
#' @keywords internal
extract_frontmatter_data <- function(rd_obj) {
  # Title: prefer \title over \name
  title <- extract_rd_text(rd_obj, "title") %||%
    extract_rd_text(rd_obj, "name") %||%
    "Documentation"

  # Clean title
  title <- gsub("[\"\n\r]", " ", title)
  title <- trimws(gsub("\\s+", " ", title))

  # Description: first sentence of \description, for SEO/previews
  desc_text <- extract_rd_text(rd_obj, "description")
  description <- NULL

  if (!is.null(desc_text)) {
    # Strip HTML tags (e.g., from lifecycle badges)
    desc_text <- gsub("<[^>]+>", "", desc_text)
    # Remove lifecycle badge remnants (URLs and image references)
    desc_text <- gsub("https?://[^\\s]+", "", desc_text)
    desc_text <- gsub("lifecycle-[a-z]+\\.svg", "", desc_text)
    desc_text <- gsub("\\[Superseded\\]|\\[Deprecated\\]|\\[Experimental\\]|\\[Stable\\]", "", desc_text)
    desc_text <- trimws(gsub("\\s+", " ", desc_text))

    # Get first sentence
    sentences <- strsplit(desc_text, "(?<=[.!?])\\s+", perl = TRUE)[[1]]
    if (length(sentences) > 0) {
      description <- sentences[1]
      # Truncate if too long
      if (nchar(description) > 160) {
        description <- paste0(substr(description, 1, 157), "...")
      }
      # Clean for YAML
      description <- gsub("[\"\n\r]", " ", description)
      description <- trimws(gsub("\\s+", " ", description))
    }
  }

  # Aliases
  aliases <- get_rd_aliases(rd_obj)

  list(
    title = title,
    description = description,
    aliases = aliases
  )
}

#' Build YAML frontmatter string
#'
#' @param frontmatter_data List from extract_frontmatter_data
#'
#' @return Character string with YAML frontmatter
#' @keywords internal
build_frontmatter <- function(frontmatter_data) {
  lines <- c("---")

  # Title (required)
  title <- gsub('"', '\\"', frontmatter_data$title)
  lines <- c(lines, paste0('title: "', title, '"'))

  # Description (optional)
  if (!is.null(frontmatter_data$description)) {
    desc <- gsub('"', '\\"', frontmatter_data$description)
    lines <- c(lines, paste0('description: "', desc, '"'))
  }

  # Enable pagefind indexing
  lines <- c(lines, "pagefind: true")

  lines <- c(lines, "---", "")
  paste(lines, collapse = "\n")
}

#' Convert a single Rd object to Markdown
#'
#' This is the main conversion function. It uses R's built-in Rd2HTML
#' converter followed by pandoc to produce clean Markdown output.
#'
#' @param rd_obj Rd object from tools::Rd_db
#' @param config List with configuration options:
#'   - skip_sections: character vector of sections to omit
#'   - arguments_format: "table" (default), "html", or "list"
#'   - include_title: logical, whether to include title as h1 (default TRUE)
#' @param output_path Path to site output directory (for example outputs)
#' @param pkg_name Package name (for link resolution)
#'
#' @return Character string containing Markdown
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' rd_db <- tools::Rd_db("dplyr")
#' md <- rd_to_markdown(rd_db[["filter.Rd"]])
#' cat(md)
#' }
rd_to_markdown <- function(
    rd_obj,
    config = list(),
    output_path = NULL,
    pkg_name = NULL) {
  require_pandoc()

  # Default configuration
  default_config <- list(
    skip_sections = c("alias", "keyword", "concept", "encoding"),
    arguments_format = "table",
    include_title = TRUE,
    include_frontmatter = TRUE
  )
  config <- utils::modifyList(default_config, config)

  # Extract frontmatter data
  fm_data <- extract_frontmatter_data(rd_obj)

  # Load package if specified (needed for \Sexpr evaluation)
  if (!is.null(pkg_name)) {
    requireNamespace(pkg_name, quietly = TRUE)
  }

  # Convert entire Rd to HTML
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  tryCatch(
    tools::Rd2HTML(rd_obj, out = tmp_html, fragment = TRUE, package = pkg_name),
    error = function(e) {
      cli::cli_abort("Failed to convert Rd to HTML: {e$message}")
    }
  )

  html_content <- paste(readLines(tmp_html, warn = FALSE), collapse = "\n")

  # Evaluate any unevaluated \Sexpr expressions
  html_content <- evaluate_sexpr(html_content)

  # Convert HTML to Markdown
  md <- html_to_md(html_content)

  # Normalize R language identifier to lowercase (pandoc may output uppercase/spaced)
  md <- gsub("``` *R\\b", "```r", md, perl = TRUE)

  # Post-processing pipeline
  md <- fix_heading_levels(md)
  md <- fix_internal_links(md, pkg_name)
  md <- convert_sourcecode_divs(md)
  md <- fix_lifecycle_badges(md)
  md <- fix_figure_paths(md)

  # Remove Title and Name sections (we handle title via frontmatter)
  md <- remove_sections(md, c("Title", "Name"))

  # Remove other skipped sections
  md <- remove_sections(md, config$skip_sections)

  # Escape < outside code blocks for MDX compatibility
  # Must happen BEFORE argument table insertion so our HTML table isn't escaped
  md <- escape_angle_brackets(md)

  # Handle arguments section specially if requested
  if (config$arguments_format == "table") {
    args_table <- arguments_to_md_table(rd_obj)
    if (!is.null(args_table)) {
      # Replace HTML tables in Arguments section (pandoc often outputs HTML tables)
      md <- gsub(
        "(?s)(## Arguments\\s*\n+)<table[^>]*>.*?</table>",
        paste0("\\1", args_table),
        md,
        perl = TRUE
      )
      # Also try markdown table format (fallback)
      md <- gsub(
        "(?s)(## Arguments\\s*\n+)\\|[^|]*\\|[^|]*\\|\\s*\n\\|[-|]+\\|\\s*\n(\\|[^\\n]+\\n)+",
        paste0("\\1", args_table, "\n"),
        md,
        perl = TRUE
      )
    }
  }

  # Handle seealso section specially to preserve \link tags
  seealso_md <- seealso_to_md(rd_obj, pkg_name)
  if (!is.null(seealso_md)) {
    md <- gsub(
      "(?s)## See Also\\s*\\n+.*?(?=\\n## |$)",
      paste0("## See Also\n\n", seealso_md, "\n\n"),
      md,
      perl = TRUE
    )
  }

  # Handle sections with equations - Rd2HTML doesn't preserve LaTeX
  equation_sections <- c("description", "details", "value")
  section_headings <- c("Description", "Details", "Value")

  for (i in seq_along(equation_sections)) {
    section_name <- equation_sections[i]
    section_heading <- section_headings[i]
    rd_section <- get_rd_section(rd_obj, section_name)

    if (!is.null(rd_section) && section_has_tag(rd_section, c("\\eqn", "\\deqn"))) {
      section_md <- rd_section_to_md(rd_section)
      if (!is.null(section_md) && nchar(section_md) > 0) {
        pattern <- paste0("(?s)## ", section_heading, "\\s*\\n+.*?(?=\\n## |$)")
        match_info <- regexpr(pattern, md, perl = TRUE)
        if (match_info > 0) {
          match_start <- match_info
          match_len <- attr(match_info, "match.length")
          new_section <- paste0("## ", section_heading, "\n\n", section_md, "\n\n")
          md <- paste0(
            substr(md, 1, match_start - 1),
            new_section,
            substr(md, match_start + match_len, nchar(md))
          )
        }
      }
    }
  }

  # Handle custom sections with \describe blocks
  for (element in rd_obj) {
    el_tag <- attr(element, "Rd_tag")
    if (!is.null(el_tag) && el_tag == "\\section" && length(element) >= 2) {
      section_title <- trimws(paste(unlist(element[[1]]), collapse = ""))
      section_content <- element[[2]]
      if (nchar(section_title) > 0 && section_has_tag(section_content, "\\describe")) {
        section_md <- rd_section_to_md(section_content)
        if (!is.null(section_md) && nchar(section_md) > 0) {
          pattern <- paste0("(?s)## ", section_title, "\\s*\\n+.*?(?=\\n## |$)")
          match_info <- regexpr(pattern, md, perl = TRUE)
          if (match_info > 0) {
            match_start <- match_info
            match_len <- attr(match_info, "match.length")
            new_section <- paste0("## ", section_title, "\n\n", section_md, "\n\n")
            md <- paste0(
              substr(md, 1, match_start - 1),
              new_section,
              substr(md, match_start + match_len, nchar(md))
            )
          }
        }
      }
    }
  }

  # Clean up excessive whitespace
  md <- gsub("\n{3,}", "\n\n", md)
  md <- trimws(md)

  # Build final output
  result_parts <- character()

  # Frontmatter
  if (config$include_frontmatter) {
    result_parts <- c(result_parts, build_frontmatter(fm_data))
  }

  # Title heading - skip if we have frontmatter (Starlight uses frontmatter title)
  if (config$include_title && !config$include_frontmatter) {
    result_parts <- c(result_parts, paste0("# ", fm_data$title), "")
  }

  # Main content
  result_parts <- c(result_parts, md)

  paste(result_parts, collapse = "\n")
}

#' Convert all Rd files from an installed package to Markdown
#'
#' @param pkg_name Name of an installed package
#' @param config Configuration list (see rd_to_markdown)
#' @param output_path Path to site output directory
#'
#' @return Named list of Markdown strings, keyed by Rd filename
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' md_files <- package_rd_to_markdown("starlightr")
#' names(md_files)
#' }
package_rd_to_markdown <- function(pkg_name, config = list(), output_path = NULL) {
  require_pandoc()

  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    cli::cli_abort("Package {.pkg {pkg_name}} is not installed.")
  }

  rd_db <- tools::Rd_db(pkg_name)

  if (length(rd_db) == 0) {
    cli::cli_warn("No Rd files found in package {.pkg {pkg_name}}.")
    return(list())
  }

  cli::cli_progress_bar("Converting Rd files", total = length(rd_db))

  results <- list()
  for (name in names(rd_db)) {
    cli::cli_progress_update()

    tryCatch(
      {
        results[[name]] <- rd_to_markdown(
          rd_db[[name]],
          config = config,
          output_path = output_path,
          pkg_name = pkg_name
        )
      },
      error = function(e) {
        cli::cli_warn("Failed to convert {name}: {e$message}")
        results[[name]] <- NULL
      }
    )
  }

  cli::cli_progress_done()
  results
}
