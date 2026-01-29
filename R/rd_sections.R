# Rd section conversion functions for starlightr

#' Extract LaTeX content from Rd element
#'
#' Rd parses LaTeX commands inside \\eqn/\\deqn as Rd tags.
#' This function reconstructs the original LaTeX by preserving
#' tag names as LaTeX commands.
#'
#' @param el Rd element
#' @return Character string with LaTeX content
#' @keywords internal
extract_latex_from_rd <- function(el) {
  if (is.character(el)) return(el)

  tag <- attr(el, "Rd_tag")

  if (is.list(el)) {
    # Get content from children
    content <- paste(vapply(el, extract_latex_from_rd, character(1)), collapse = "")

    # If this has a tag that looks like a LaTeX command, reconstruct it
    if (!is.null(tag) && nchar(tag) > 0) {
      # Skip Rd-specific tags that aren't LaTeX commands
      if (tag %in% c("TEXT", "RCODE", "VERB", "COMMENT", "LIST")) {
        return(content)
      }

      # Check if tag starts with backslash (LaTeX command)
      if (substring(tag, 1, 1) == "\\") {
        # Get the command name (without backslash)
        cmd_name <- substring(tag, 2)
        # Reconstruct with explicit backslash
        if (nchar(content) > 0) {
          return(paste0("\\", cmd_name, "{", content, "}"))
        } else {
          # Command without content (like \theta, \alpha)
          return(paste0("\\", cmd_name))
        }
      }
    }

    return(content)
  }

  ""
}

#' Process an Rd section to markdown
#'
#' Converts an Rd section directly to markdown, handling equations,
#' describe blocks, and other Rd markup. This bypasses Rd2HTML for
#' sections that need special handling.
#'
#' @param rd_section An Rd section
#' @return Markdown string
#' @keywords internal
rd_section_to_md <- function(rd_section) {
  if (is.null(rd_section)) return(NULL)

  # Helper to normalize inline content (collapse all whitespace to single space)
  normalize_inline <- function(text) {
    text <- gsub("[\n\r\t ]+", " ", text, perl = TRUE)
    trimws(text)
  }

  convert_element <- function(el) {
    if (is.character(el)) {
      # Return character content with whitespace normalized
      return(gsub("[\n\r\t ]+", " ", el, perl = TRUE))
    }

    tag <- attr(el, "Rd_tag")

    if (!is.null(tag)) {
      if (tag == "\\eqn") {
        latex <- if (length(el) >= 1) extract_latex_from_rd(el[[1]]) else ""
        # Inline math - single line, no surrounding whitespace
        return(paste0("$", trimws(latex), "$"))
      } else if (tag == "\\deqn") {
        latex <- if (length(el) >= 1) extract_latex_from_rd(el[[1]]) else ""
        # Display math with $$ - preserve internal newlines
        return(paste0("\n\n$$\n", latex, "\n$$\n\n"))
      } else if (tag == "\\code") {
        inner <- paste0(vapply(el, convert_element, character(1)), collapse = "")
        # If inner already contains a markdown link (from \link), don't double-wrap
        if (grepl("\\]\\(", inner)) {
          return(inner)
        }
        return(paste0("`", normalize_inline(inner), "`"))
      } else if (tag == "\\emph" || tag == "\\var") {
        inner <- paste0(vapply(el, convert_element, character(1)), collapse = "")
        return(paste0("*", normalize_inline(inner), "*"))
      } else if (tag == "\\strong" || tag == "\\bold") {
        inner <- paste0(vapply(el, convert_element, character(1)), collapse = "")
        return(paste0("**", normalize_inline(inner), "**"))
      } else if (tag == "\\link" || tag == "\\linkS4class") {
        inner <- paste0(vapply(el, convert_element, character(1)), collapse = "")
        inner <- normalize_inline(inner)
        # Extract target from link text (remove parentheses for function calls)
        target <- gsub("[()]", "", inner)
        # Sanitize: lowercase and replace dots with hyphens for Astro slugs
        target_slug <- gsub(".", "-", tolower(target), fixed = TRUE)
        return(paste0("[`", inner, "`](../", target_slug, "/)"))
      } else if (tag == "\\sQuote") {
        inner <- paste0(vapply(el, convert_element, character(1)), collapse = "")
        return(paste0("'", normalize_inline(inner), "'"))
      } else if (tag == "\\dQuote") {
        inner <- paste0(vapply(el, convert_element, character(1)), collapse = "")
        return(paste0("\"", normalize_inline(inner), "\""))
      } else if (tag == "TEXT" || tag == "RCODE") {
        text <- paste(unlist(el), collapse = "")
        return(gsub("[\n\r\t ]+", " ", text, perl = TRUE))
      } else if (grepl("itemize|enumerate", tag, ignore.case = TRUE)) {
        # In Rd itemize/enumerate, \item is a marker tag and content follows as siblings
        # Group content between \item markers into list items
        items <- character()
        current_content <- character()
        in_item <- FALSE

        for (child in el) {
          child_tag <- attr(child, "Rd_tag")
          is_item_marker <- !is.null(child_tag) && grepl("^\\\\?item$", child_tag, ignore.case = TRUE)

          if (is_item_marker) {
            # Save previous item if we have content
            if (in_item && length(current_content) > 0) {
              item_text <- paste(current_content, collapse = "")
              item_text <- gsub("[\n\r\t ]+", " ", item_text, perl = TRUE)
              item_text <- trimws(item_text)
              if (nchar(item_text) > 0) {
                items <- c(items, paste0("- ", item_text))
              }
            }
            current_content <- character()
            in_item <- TRUE
          } else if (in_item) {
            # Accumulate content for current item
            current_content <- c(current_content, convert_element(child))
          }
          # Content before first \item is ignored (usually just whitespace)
        }

        # Handle last item
        if (in_item && length(current_content) > 0) {
          item_text <- paste(current_content, collapse = "")
          item_text <- gsub("[\n\r\t ]+", " ", item_text, perl = TRUE)
          item_text <- trimws(item_text)
          if (nchar(item_text) > 0) {
            items <- c(items, paste0("- ", item_text))
          }
        }

        if (length(items) > 0) {
          return(paste0("\n", paste(items, collapse = "\n"), "\n"))
        }
        return("")
      } else if (grepl("^\\\\?item$", tag, ignore.case = TRUE)) {
        # For describe-style items: \item{term}{definition}
        # First child is term, rest is definition
        if (length(el) >= 2) {
          term <- paste0(vapply(el[[1]], convert_element, character(1)), collapse = "")
          term <- trimws(term)

          def_parts <- vapply(el[-1], convert_element, character(1))
          definition <- paste0(def_parts, collapse = "")
          definition <- gsub("[\n\r\t ]+", " ", definition, perl = TRUE)
          definition <- trimws(definition)

          if (nchar(term) > 0) {
            return(paste0("- **", term, "**: ", definition))
          } else {
            return(paste0("- ", definition))
          }
        } else if (length(el) >= 1) {
          # Just content, no separate term
          inner <- paste0(vapply(el, convert_element, character(1)), collapse = "")
          inner <- gsub("[\n\r\t ]+", " ", inner, perl = TRUE)
          inner <- trimws(inner)
          if (nchar(inner) == 0) return("")
          return(paste0("- ", inner))
        }
        return("")
      } else if (tag == "\\cr") {
        return(" ")  # Convert to space in inline context
      } else if (tag == "\\dots" || tag == "\\ldots") {
        return("...")
      } else if (tag == "\\file" || tag == "\\pkg" || tag == "\\samp") {
        inner <- paste0(vapply(el, convert_element, character(1)), collapse = "")
        return(paste0("`", normalize_inline(inner), "`"))
      } else if (tag == "\\url") {
        url <- paste(unlist(el), collapse = "")
        return(paste0("[", url, "](", url, ")"))
      } else if (tag == "\\href") {
        url <- if (length(el) >= 1) paste(unlist(el[[1]]), collapse = "") else ""
        text <- if (length(el) >= 2) paste0(vapply(el[[2]], convert_element, character(1)), collapse = "") else url
        return(paste0("[", normalize_inline(text), "](", url, ")"))
      } else if (tag == "\\describe") {
        items <- vapply(el, convert_element, character(1))
        return(paste(items, collapse = "\n\n"))
      }
    }

    # Default: recurse but preserve newlines from list items
    if (is.list(el)) {
      parts <- vapply(el, convert_element, character(1))
      inner <- paste0(parts, collapse = "")
      # Only normalize spaces/tabs, preserve newlines
      inner <- gsub("[ \t]+", " ", inner, perl = TRUE)
      return(inner)
    }

    ""
  }

  result <- convert_element(rd_section)
  # Clean up multiple newlines but preserve paragraph breaks
  result <- gsub("\n{3,}", "\n\n", result, perl = TRUE)
  trimws(result)
}

#' Convert seealso section to Markdown with proper links
#'
#' Rd2HTML strips link tags, so we need to process seealso directly
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
      # Use relative paths from /reference/current/ to /reference/target/
      # Trailing slash for trailingSlash: 'always' compatibility
      if (code_wrap) {
        return(paste0('[`', text, '`](../', tolower(target), '/)'))
      } else {
        return(paste0('[', text, '](../', tolower(target), '/)'))
      }
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

#' Convert Rd arguments section to HTML table
#'
#' @param rd_obj Rd object
#'
#' @return HTML table string or NULL if no arguments
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
      desc_md <- fix_lifecycle_badges(desc_md)

      # Flatten description to single line for table cell
      # Replace newlines with spaces, clean up
      desc_md <- gsub("\n+", " ", desc_md)
      desc_md <- trimws(gsub("\\s+", " ", desc_md))

      # For HTML table, convert markdown formatting to HTML
      # Code: `text` -> <code>text</code>
      desc_html <- gsub("`([^`]+)`", "<code>\\1</code>", desc_md)

      args_list[[length(args_list) + 1]] <- list(
        name = name,
        description = desc_html
      )
    }
  }

  if (length(args_list) == 0) {
    return(NULL)
  }

  # Build HTML table wrapped in div for CSS targeting
  # MDX doesn't parse markdown inside HTML blocks, so we use pure HTML
  lines <- c(
    "",
    "<div class=\"arg-table\">",
    "<table>",
    "<thead>",
    "<tr>",
    "<th>Argument</th>",
    "<th>Description</th>",
    "</tr>",
    "</thead>",
    "<tbody>"
  )

  for (arg in args_list) {
    # Escape HTML special chars in name
    name_escaped <- gsub("&", "&amp;", arg$name)
    name_escaped <- gsub("<", "&lt;", name_escaped)
    name_escaped <- gsub(">", "&gt;", name_escaped)

    lines <- c(
      lines,
      "<tr>",
      paste0("<td><code>", name_escaped, "</code></td>"),
      paste0("<td>", arg$description, "</td>"),
      "</tr>"
    )
  }

  lines <- c(lines, "</tbody>", "</table>", "</div>", "")

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
