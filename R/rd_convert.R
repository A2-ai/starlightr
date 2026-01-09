#' @title Rd to Markdown Conversion
#' @description Convert R documentation (Rd) files to Markdown using R's built-in
#'   Rd2HTML converter and pandoc.
#' @name rd_convert
NULL

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Check if pandoc is available
#'
#' @return Logical, TRUE if pandoc is available
#' @keywords internal
check_pandoc <- function() {
 if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    return(FALSE)
  }
  rmarkdown::pandoc_available()
}
#' Ensure pandoc is available, error if not
#'
#' @return NULL (called for side effect)
#' @keywords internal
require_pandoc <- function() {
  if (!check_pandoc()) {
    cli::cli_abort(c(
      "Pandoc is required but not found.",
      "i" = "Install pandoc: {.url https://pandoc.org/installing.html}",
      "i" = "Or install the {.pkg rmarkdown} package which includes pandoc."
    ))
  }
  invisible(NULL)
}

#' Convert HTML string to Markdown using pandoc
#'
#' @param html Character string containing HTML
#' @param wrap Logical, whether to wrap long lines (default FALSE)
#'
#' @return Character string containing Markdown
#' @keywords internal
html_to_md <- function(html, wrap = FALSE) {
  require_pandoc()

  if (is.null(html) || length(html) == 0 || nchar(trimws(html)) == 0) {
    return("")
  }

  wrap_arg <- if (wrap) "--wrap=auto" else "--wrap=none"

  result <- tryCatch(
    {
      system2(
        rmarkdown::pandoc_exec(),
        args = c("-f", "html", "-t", "gfm", wrap_arg),
        input = html,
        stdout = TRUE,
        stderr = FALSE
      )
    },
    error = function(e) {
      cli::cli_warn("Pandoc conversion failed: {e$message}")
      # Fallback: strip HTML tags
      gsub("<[^>]+>", "", html)
    }
  )

  paste(result, collapse = "\n")
}

#' Get a specific section from an Rd object
#'
#' @param rd_obj Rd object from tools::Rd_db
#' @param tag Section tag (e.g., "description", "arguments")
#'
#' @return The Rd element for that section, or NULL if not found
#' @keywords internal
get_rd_section <- function(rd_obj, tag) {
  if (!startsWith(tag, "\\")) {
    tag <- paste0("\\", tag)
  }

  for (element in rd_obj) {
    element_tag <- attr(element, "Rd_tag")
    if (!is.null(element_tag) && element_tag == tag) {
      return(element)
    }
  }
  NULL
}

#' Get all aliases from an Rd object
#'
#' @param rd_obj Rd object from tools::Rd_db
#'
#' @return Character vector of aliases
#' @keywords internal
get_rd_aliases <- function(rd_obj) {
  aliases <- character()
  for (element in rd_obj) {
    element_tag <- attr(element, "Rd_tag")
    if (!is.null(element_tag) && element_tag == "\\alias") {
      alias <- trimws(paste(unlist(element), collapse = ""))
      aliases <- c(aliases, alias)
    }
  }
  aliases
}

#' Extract plain text from an Rd section (for frontmatter)
#'
#' @param rd_obj Rd object
#' @param section Section name
#'
#' @return Plain text string or NULL
#' @keywords internal
extract_rd_text <- function(rd_obj, section) {
  element <- get_rd_section(rd_obj, section)
  if (is.null(element)) {
    return(NULL)
  }

  # Flatten to text, removing Rd markup
  text <- paste(unlist(element), collapse = " ")
  text <- trimws(gsub("\\s+", " ", text))

  if (nchar(text) == 0) NULL else text
}

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

  lines <- c(lines, "---", "")
  paste(lines, collapse = "\n")
}

#' Convert Rd arguments section to Markdown table
#'
#' @param rd_obj Rd object
#'
#' @return Markdown table string or NULL if no arguments
#' @keywords internal
arguments_to_md_table <- function(rd_obj) {
  args_el <- get_rd_section(rd_obj, "arguments")
  if (is.null(args_el)) {
    return(NULL)
  }

  args_list <- list()

  for (el in args_el) {
    el_tag <- attr(el, "Rd_tag")
    if (!is.null(el_tag) && el_tag == "\\item" && length(el) >= 2) {
      # First element is argument name(s)
      # Second element is description

      # Convert name to text
      # Handle special case: \dots tag becomes "..."
      name_el <- el[[1]]
      if (length(name_el) == 1 && is.list(name_el[[1]])) {
        inner_tag <- attr(name_el[[1]], "Rd_tag")
        if (!is.null(inner_tag) && inner_tag == "\\dots") {
          name <- "..."
        } else {
          name_parts <- unlist(name_el)
          name <- trimws(paste(name_parts, collapse = ""))
        }
      } else {
        name_parts <- unlist(name_el)
        name <- trimws(paste(name_parts, collapse = ""))
      }

      # Convert description to HTML then to Markdown
      desc_html <- tryCatch(
        {
          tmp <- tempfile(fileext = ".html")
          on.exit(unlink(tmp), add = TRUE)

          # Wrap in minimal Rd structure for Rd2HTML
          # Actually, we can capture the output directly
          utils::capture.output(
            tools::Rd2HTML(el[[2]], fragment = TRUE, out = stdout()),
            type = "output"
          ) |> paste(collapse = "\n")
        },
        error = function(e) {
          paste(unlist(el[[2]]), collapse = " ")
        }
      )

      desc_md <- html_to_md(desc_html)

      # Fix lifecycle badge paths to use CDN
      # Match paths like ../help/figures/lifecycle-*.svg (careful not to eat markdown syntax)
      desc_md <- gsub(
        "\\.\\.(/[^)\\s]+)?/lifecycle-([a-z]+)\\.svg",
        "https://lifecycle.r-lib.org/articles/figures/lifecycle-\\2.svg",
        desc_md,
        perl = TRUE
      )

      # Flatten description to single line for table cell
      # Replace newlines with spaces, clean up
      desc_md <- gsub("\n+", " ", desc_md)
      desc_md <- trimws(gsub("\\s+", " ", desc_md))

      # Escape pipe characters for markdown table
      desc_md <- gsub("\\|", "\\\\|", desc_md)

      # Escape < and > for MDX using HTML entities
      desc_md <- gsub("<", "&lt;", desc_md)
      desc_md <- gsub(">", "&gt;", desc_md)

      args_list[[length(args_list) + 1]] <- list(
        name = name,
        description = desc_md
      )
    }
  }

  if (length(args_list) == 0) {
    return(NULL)
  }

  # Build markdown table
  lines <- c(
    "| Argument | Description |",
    "|----------|-------------|"
  )

  for (arg in args_list) {
    # Escape backticks in name if needed, wrap in code
    name_clean <- gsub("`", "\\\\`", arg$name)
    lines <- c(lines, sprintf("| `%s` | %s |", name_clean, arg$description))
  }

  paste(lines, collapse = "\n")
}

#' Convert Rd code section (usage/examples) to Markdown code block
#'
#' @param rd_obj Rd object
#' @param section Section name ("usage" or "examples")
#'
#' @return Markdown code block string or NULL
#' @keywords internal
code_section_to_md <- function(rd_obj, section) {
  element <- get_rd_section(rd_obj, section)
  if (is.null(element)) {
    return(NULL)
  }

  # Extract raw code text
  # For code sections, we want the raw R code, not HTML-converted
  code_parts <- character()

  extract_code <- function(el) {
    tag <- attr(el, "Rd_tag")

    if (is.character(el)) {
      return(el)
    }

    if (is.list(el)) {
      # Handle special tags
      if (!is.null(tag)) {
        if (tag == "\\dontrun") {
          # Include but mark as dontrun
          inner <- sapply(el, extract_code) |> paste(collapse = "")
          return(paste0("# Not run:\n# ", gsub("\n", "\n# ", inner)))
        } else if (tag == "\\donttest") {
          # Include normally
          return(sapply(el, extract_code) |> paste(collapse = ""))
        } else if (tag == "\\dontshow") {
          # Skip entirely
          return("")
        } else if (tag %in% c("RCODE", "VERB", "TEXT")) {
          return(paste(unlist(el), collapse = ""))
        } else if (tag == "\\method" || tag == "\\S3method") {
          # S3 method: \method{generic}{class}
          if (length(el) >= 2) {
            generic <- paste(unlist(el[[1]]), collapse = "")
            class <- paste(unlist(el[[2]]), collapse = "")
            return(paste0("## S3 method for class '", class, "'\n", generic))
          }
        } else if (tag == "\\S4method") {
          if (length(el) >= 2) {
            generic <- paste(unlist(el[[1]]), collapse = "")
            sig <- paste(unlist(el[[2]]), collapse = "")
            return(paste0("## S4 method for signature '", sig, "'\n", generic))
          }
        }
      }

      # Recurse into children
      return(sapply(el, extract_code) |> paste(collapse = ""))
    }

    ""
  }

  code <- extract_code(element)
  code <- trimws(code)

  if (nchar(code) == 0) {
    return(NULL)
  }

  paste0("```r\n", code, "\n```")
}

#' Get example outputs for embedding
#'
#' @param func_name Function name
#' @param output_path Path to site output directory
#'
#' @return Markdown string with embedded outputs or NULL
#' @keywords internal
get_example_outputs <- function(func_name, output_path) {
  if (is.null(func_name) || is.null(output_path)) {
    return(NULL)
  }

  outputs <- character()

  # PNG (ggplot outputs)
  png_file <- file.path(output_path, "public", "examples", paste0(func_name, ".png"))
  if (file.exists(png_file)) {
    rel_path <- file.path("/examples", paste0(func_name, ".png"))
    outputs <- c(outputs, paste0("![Example output](", rel_path, ")"))
  }

  # HTML (gt table outputs)
  html_file <- file.path(output_path, "public", "examples", paste0(func_name, ".html"))
  if (file.exists(html_file)) {
    tryCatch(
      {
        html_content <- readLines(html_file, warn = FALSE)
        outputs <- c(outputs, paste(html_content, collapse = "\n"))
      },
      error = function(e) NULL
    )
  }

  # Text output
  text_file <- file.path(output_path, "public", "examples", "text", paste0(func_name, ".txt"))
  if (file.exists(text_file)) {
    tryCatch(
      {
        text_content <- readLines(text_file, warn = FALSE)
        if (length(text_content) > 0) {
          outputs <- c(outputs, paste0("```\n", paste(text_content, collapse = "\n"), "\n```"))
        }
      },
      error = function(e) NULL
    )
  }

  if (length(outputs) > 0) {
    return(paste(outputs, collapse = "\n\n"))
  }

  NULL
}

#' Convert seealso section to Markdown with proper links
#'
#' Rd2HTML strips \link tags, so we need to process seealso directly
#' from the Rd object to preserve internal links.
#'
#' @param rd_obj Rd object
#' @param pkg_name Package name for resolving internal links
#' @return Markdown string or NULL if no seealso section
#' @keywords internal
seealso_to_md <- function(rd_obj, pkg_name = NULL) {
  seealso <- get_rd_section(rd_obj, "seealso")
  if (is.null(seealso)) return(NULL)

  # Find \link child within an element (for \code{\link{...}} pattern)
  find_link_child <- function(el) {
    if (!is.list(el)) return(NULL)
    for (child in el) {
      if (is.list(child)) {
        child_tag <- attr(child, "Rd_tag")
        if (!is.null(child_tag) && child_tag == "\\link") {
          return(child)
        }
      }
    }
    NULL
  }

  # Process a \link element into markdown
  process_link <- function(link_el, code_wrap = FALSE) {
    option <- attr(link_el, "Rd_option")
    text <- convert_children(link_el)
    text <- trimws(text)

    # Determine if internal or external link
    is_internal <- TRUE
    target <- NULL

    if (!is.null(option)) {
      option_text <- as.character(option)
      if (startsWith(option_text, "=")) {
        # \link[=topic]{text} - internal link
        target <- substring(option_text, 2)
      } else if (grepl(":", option_text, fixed = TRUE)) {
        # \link[pkg:topic]{text} - external
        is_internal <- FALSE
      } else {
        # \link[pkg]{func} - external
        is_internal <- FALSE
      }
    } else {
      # \link{topic} - internal, target is the text (without parentheses)
      target <- gsub("[()]", "", text)
    }

    if (is_internal && !is.null(target)) {
      link_text <- if (code_wrap) paste0("`", text, "`") else text
      return(paste0("[", link_text, "](./", tolower(target), ")"))
    } else {
      # External or couldn't determine - just code format
      return(paste0("`", text, "`"))
    }
  }

  # Recursively convert Rd elements to markdown
  convert_element <- function(el) {
    if (is.character(el)) return(el)

    tag <- attr(el, "Rd_tag")

    if (is.null(tag)) {
      # List without tag - recurse into children
      return(convert_children(el))
    }

    if (tag == "TEXT") {
      return(paste(unlist(el), collapse = ""))
    }

    if (tag == "\\link") {
      return(process_link(el, code_wrap = FALSE))
    }

    if (tag == "\\code") {
      # Check if this is \code{\link{...}} pattern
      link_child <- find_link_child(el)
      if (!is.null(link_child)) {
        return(process_link(link_child, code_wrap = TRUE))
      }
      # Regular code - wrap in backticks
      inner <- convert_children(el)
      return(paste0("`", inner, "`"))
    }

    if (tag == "\\emph") {
      inner <- convert_children(el)
      return(paste0("*", inner, "*"))
    }

    if (tag == "\\strong") {
      inner <- convert_children(el)
      return(paste0("**", inner, "**"))
    }

    # Default: just process children
    return(convert_children(el))
  }

  convert_children <- function(el) {
    if (!is.list(el)) return(as.character(el))
    paste(sapply(el, convert_element), collapse = "")
  }

  md_content <- convert_children(seealso)
  md_content <- trimws(md_content)

  if (nchar(md_content) == 0) return(NULL)

  md_content
}

#' Fix internal package links in Markdown
#'
#' Transforms links produced by Rd2HTML to work within the Starlight site.
#' Internal links get lowercase slugs without extension for Starlight compatibility.
#'
#' @param md Markdown string
#' @param pkg_name Package name (optional, for context)
#'
#' @return Markdown string with fixed links
#' @keywords internal
fix_internal_links <- function(md, pkg_name = NULL) {
  # Internal help links: [text](../help/topic.html) → [`text`](./lowercase-topic)
  # Use lowercase slugs without extension for Starlight compatibility
  # Wrap link text in backticks for code formatting
  md <- gsub(
    "\\[([^]]+)\\]\\(\\.\\./help/([^)]+)\\.html\\)",
    "[`\\1`](./\\L\\2)",
    md,
    perl = TRUE
  )

  # Also handle: [text](../../pkgname/help/topic.html) for same package
  if (!is.null(pkg_name)) {
    pattern <- paste0("\\[([^]]+)\\]\\(\\.\\./\\.\\./", pkg_name, "/help/([^)]+)\\.html\\)")
    md <- gsub(pattern, "[`\\1`](./\\L\\2)", md, perl = TRUE)
  }

  # External package links become plain code text (no link)
  md <- gsub(
    "\\[([^]]+)\\]\\(\\.\\./\\.\\./[^)]+\\)",
    "`\\1`",
    md
  )

  md
}

#' Fix heading levels in Markdown
#'
#' Removes the first h1 (title duplicate). Keeps sections as h2 for Starlight TOC.
#' Rd2HTML produces h3 headings which become h2 after pandoc.
#'
#' @param md Markdown string
#'
#' @return Markdown string with adjusted headings
#' @keywords internal
fix_heading_levels <- function(md) {

  # Remove the first h1 heading (duplicates frontmatter title)
  md <- gsub("^# [^\n]+\n+", "", md)
  md <- gsub("^\n*# [^\n]+\n+", "", md)

  # Convert h3 to h2 (keep sections as h2 for Starlight TOC)
  md <- gsub("^### ", "## ", md)
  md <- gsub("\n### ", "\n## ", md)
  # Note: h2 sections stay as h2 (not promoted to h1) so they appear in TOC

  md
}

#' Convert sourceCode divs to fenced code blocks
#'
#' Rd2HTML outputs code as <div class="sourceCode r">...</div> which pandoc
#' preserves as raw HTML. This converts them to proper markdown code fences.
#'
#' @param md Markdown string
#' @return Markdown string with divs converted to code fences
#' @keywords internal
convert_sourcecode_divs <- function(md) {
  # Match <div class="sourceCode r"> or <div class="sourceCode"> blocks
  # and convert to ```r fenced code blocks
  pattern <- '(?s)<div class="sourceCode[^"]*">\\s*(.*?)\\s*</div>'

  md <- gsub(pattern, "```r\n\\1\n```", md, perl = TRUE)

  md
}

#' Evaluate unevaluated Sexpr expressions in HTML
#'
#' Rd2HTML doesn't evaluate stage=render Sexprs when converting statically.
#' This function finds and evaluates them manually.
#'
#' @param html HTML string potentially containing unevaluated Sexpr
#' @return HTML with Sexpr expressions evaluated
#' @keywords internal
evaluate_sexpr <- function(html) {
  # Pattern matches \Sexpr[...]{code}
  pattern <- "\\\\Sexpr\\[([^\\]]+)\\]\\{([^}]+)\\}"

  # Find all matches
  matches <- gregexpr(pattern, html, perl = TRUE)[[1]]

  if (matches[1] == -1) {
    return(html)
  }

  # Process each match from last to first (to preserve positions)
  match_lengths <- attr(matches, "match.length")

  for (i in rev(seq_along(matches))) {
    start <- matches[i]
    end <- start + match_lengths[i] - 1
    full_match <- substr(html, start, end)

    # Extract the R code
    code_match <- regmatches(full_match, regexec(pattern, full_match, perl = TRUE))[[1]]
    if (length(code_match) < 3) next

    r_code <- code_match[3]

    # Try to evaluate the R expression
    result <- tryCatch(
      {
        eval(parse(text = r_code))
      },
      error = function(e) NULL
    )

    if (!is.null(result)) {
      # Convert \code{} to <code> for HTML
      result <- gsub("\\\\code\\{([^}]+)\\}", "<code>\\1</code>", result)
      # Replace the Sexpr with the result
      html <- paste0(
        substr(html, 1, start - 1),
        result,
        substr(html, end + 1, nchar(html))
      )
    }
  }

  html
}

#' Escape angle brackets outside code blocks for MDX
#'
#' @param md Markdown string
#' @return Markdown with < escaped outside code blocks
#' @keywords internal
escape_angle_brackets <- function(md) {
  # Split by code blocks, escape < only in non-code parts
  parts <- strsplit(md, "(```[^`]*```)", perl = TRUE)[[1]]
  code_blocks <- regmatches(md, gregexpr("```[^`]*```", md, perl = TRUE))[[1]]

  result <- character()
  for (i in seq_along(parts)) {
    part <- parts[i]

    # Convert <URL> autolinks to [URL](URL) format
    part <- gsub("<(https?://[^>]+)>", "[\\1](\\1)", part, perl = TRUE)

    # Escape </ (closing tags) using HTML entity
    part <- gsub("</", "&lt;/", part, fixed = TRUE)

    # Escape < followed by letter or backtick (looks like tag/JSX) using HTML entity
    part <- gsub("<(?=[a-zA-Z`])", "&lt;", part, perl = TRUE)

    # Escape > that follows word chars (closing bracket of tag-like patterns)
    # But not > at start of line (markdown blockquotes)
    part <- gsub("(?<=[a-zA-Z0-9`])>", "&gt;", part, perl = TRUE)

    result <- c(result, part)
    if (i <= length(code_blocks)) {
      result <- c(result, code_blocks[i])
    }
  }

  paste(result, collapse = "")
}

#' Remove unwanted sections from Markdown
#'
#' @param md Markdown string
#' @param skip_sections Character vector of section names to remove
#'
#' @return Markdown string with sections removed
#' @keywords internal
remove_sections <- function(md, skip_sections) {
  if (length(skip_sections) == 0) {
    return(md)
  }

  for (section in skip_sections) {
    # Match ## Section through to next ## or end of string
    # Case insensitive matching for section name
    pattern <- paste0(
      "(?m)^## ", section, "\\s*\n",
      "(?:(?!^## ).)*"
    )
    md <- gsub(pattern, "", md, perl = TRUE, ignore.case = TRUE)
  }

  # Clean up multiple blank lines
  md <- gsub("\n{3,}", "\n\n", md)

  md
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
#' @export
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

  # Post-processing
  md <- fix_heading_levels(md)
  md <- fix_internal_links(md, pkg_name)
  md <- convert_sourcecode_divs(md)

  # Fix image paths

  # Lifecycle badges: use CDN URLs instead of local paths
  # Match paths like ../help/figures/lifecycle-*.svg
  md <- gsub(
    "\\.\\.(/[^)\\s]+)?/lifecycle-([a-z]+)\\.svg",
    "https://lifecycle.r-lib.org/articles/figures/lifecycle-\\2.svg",
    md,
    perl = TRUE
  )

  # Other figures: R help uses ../help/figures/, we use /figures/
  md <- gsub("\\.\\./help/figures/", "/figures/", md)

  # Remove Title and Name sections (we handle title via frontmatter)
  md <- remove_sections(md, c("Title", "Name"))

  # Remove other skipped sections
  md <- remove_sections(md, config$skip_sections)

  # Handle arguments section specially if requested
  if (config$arguments_format == "table") {
    args_table <- arguments_to_md_table(rd_obj)
    if (!is.null(args_table)) {
      # Replace HTML tables in Arguments section (pandoc often outputs HTML tables)
      # Match from ## Arguments to the closing </table> tag
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

  # Note: Example outputs are now appended in write_md_files using MDX imports

  # Handle seealso section specially to preserve \link tags
  # (Rd2HTML strips them, so we process directly from Rd object)
  seealso_md <- seealso_to_md(rd_obj, pkg_name)
  if (!is.null(seealso_md)) {
    # Replace existing See Also section with our version that has proper links
    md <- gsub(
      "(?s)## See Also\\s*\\n+.*?(?=\\n## |$)",
      paste0("## See Also\n\n", seealso_md, "\n\n"),
      md,
      perl = TRUE
    )
  }

  # Escape < outside code blocks for MDX compatibility
  # Match < followed by a letter (looks like a tag) but not inside ```
  md <- escape_angle_brackets(md)

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
#' @export
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
