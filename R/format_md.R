#' Formats the rd file contents to markdown
#'
#' @param content rd_file contents from extract_rd_content
#' @param sections character vector of sections to include, in order
#' @param code_sections character vector of sections to format as code blocks
#' @param skip_sections character vector of sections to skip entirely
#'
#' @return markdown formatted string of content
#' @export
#'
#' @examples \dontrun{
#' rd_files <- extract_package_rd_content("rdstarlight")
#' format_md(rd_files[["function_name.Rd"]])
#' }
format_md <- function(
  content,
  sections = c(
    "description",
    "usage",
    "arguments",
    "details",
    "value",
    "examples",
    "references",
    "note",
    "author",
    "source",
    "format",
    "section",
    "subsection"
  ),
  code_sections = c("usage", "examples"),
  skip_sections = c("name", "title", "seealso")
) {
  # Start building markdown
  md_parts <- character(0)

  # Use name as main heading if available and not in skip_sections
  if (!is.null(content$name) && !"name" %in% skip_sections) {
    md_parts <- c(md_parts, paste0("# ", clean_text(content$name), "\n\n"))
  } else if (!is.null(content$title) && !"title" %in% skip_sections) {
    md_parts <- c(md_parts, paste0("# ", clean_text(content$title), "\n\n"))
  }

  # Process sections in the specified order
  for (section_name in sections) {
    if (section_name %in% skip_sections || is.null(content[[section_name]])) {
      next
    }

    section_content <- content[[section_name]]

    # Format section based on type
    if (section_name == "arguments") {
      md_parts <- c(md_parts, format_arguments_section(section_content))
    } else if (section_name %in% code_sections) {
      md_parts <- c(
        md_parts,
        format_code_section(section_name, section_content)
      )
    } else {
      md_parts <- c(
        md_parts,
        format_text_section(section_name, section_content)
      )
    }
  }

  return(paste(md_parts, collapse = ""))
}

#' Clean text content minimally - just basic whitespace cleanup
#'
#' @param text raw text content
#'
#' @return cleaned text
clean_text <- function(text) {
  if (is.null(text) || length(text) == 0) return("")

  # Convert to character if not already
  text <- as.character(text)

  # Just trim leading/trailing whitespace - don't mess with internal formatting
  text <- trimws(text)

  return(text)
}

#' Format code sections with proper line breaks
#'
#' @param section_name name of the section
#' @param section_content content of the section
#'
#' @return formatted markdown string
format_code_section <- function(section_name, section_content) {
  # Capitalize first letter for title
  title <- switch(
    section_name,
    "usage" = "Usage",
    "examples" = "Examples",
    paste0(toupper(substring(section_name, 1, 1)), substring(section_name, 2))
  )

  clean_content <- clean_text(section_content)

  # For usage, try to add some line breaks for readability
  if (section_name == "usage") {
    # Add line breaks after commas in function calls for better formatting
    clean_content <- gsub(",\\s*", ",\n  ", clean_content)
    # Fix opening parenthesis
    clean_content <- gsub("\\(\\s*", "(\n  ", clean_content)
    # Fix closing parenthesis
    clean_content <- gsub("\\s*\\)", "\n)", clean_content)
  }

  return(paste0("## ", title, "\n\n```r\n", clean_content, "\n```\n\n"))
}

#' Format regular text sections - minimal processing
#'
#' @param section_name name of the section
#' @param section_content content of the section
#'
#' @return formatted markdown string
format_text_section <- function(section_name, section_content) {
  # Capitalize first letter and handle special cases
  title <- switch(
    section_name,
    "seealso" = "See Also",
    "value" = "Returns",
    "details" = "Details",
    paste0(toupper(substring(section_name, 1, 1)), substring(section_name, 2))
  )

  # Apply special formatting for details section
  if (section_name == "details") {
    clean_content <- format_details_section(section_content)
  } else {
    clean_content <- clean_text(section_content)
  }

  return(paste0("## ", title, "\n\n", clean_content, "\n\n"))
}

#' Format details section with minimal processing
#'
#' @param details_content raw details content
#'
#' @return formatted details text
format_details_section <- function(details_content) {
  if (is.null(details_content)) return("")

  # Clean basic whitespace and split into lines
  text <- clean_text(details_content)
  lines <- unlist(strsplit(text, "\n"))

  processed_lines <- character()

  for (line in lines) {
    line <- trimws(line)

    # Skip empty lines
    if (nchar(line) == 0) {
      processed_lines <- c(processed_lines, "")
      next
    }

    # Check if line looks like it starts a list item (very general)
    if (looks_like_list_item(line)) {
      processed_lines <- c(processed_lines, paste0("- ", line))
    } else {
      processed_lines <- c(processed_lines, line)
    }
  }

  # Join back and clean up extra blank lines
  result <- paste(processed_lines, collapse = "\n")
  result <- gsub("\n\n\n+", "\n\n", result)

  return(result)
}

#' Check if a line looks like it could be a list item
#'
#' @param line text line to check
#'
#' @return logical
looks_like_list_item <- function(line) {
  # Very general: starts with number/letter followed by common separators
  # This catches: "1 =", "1:", "1.", "a)", "•", etc.
  grepl("^\\s*[0-9a-zA-Z]+\\s*[=:.)]", line)
}

#' Format arguments section as a table
#'
#' @param arguments_content raw arguments content from Rd file
#'
#' @return formatted markdown string
format_arguments_section <- function(arguments_content) {
  tryCatch(
    {
      args_df <- process_arguments(arguments_content)

      # Check if the result looks reasonable (not overly split)
      if (is.data.frame(args_df) && nrow(args_df) > 0 && ncol(args_df) >= 2) {
        # Only use if it looks like proper parsing (reasonable number of rows)
        if (nrow(args_df) <= 20) {
          # Reasonable threshold
          table_content <- paste0(
            "| Name | Description |\n",
            "|------|-------------|\n",
            paste(
              apply(args_df, 1, function(row) {
                name <- clean_text(row[1])
                desc <- clean_text(row[2])
                paste0("| `", name, "` | ", desc, " |")
              }),
              collapse = "\n"
            )
          )
          return(paste0("## Arguments\n\n", table_content, "\n\n"))
        }
      }
    },
    error = function(e) {
      # Fall through to simple format
    }
  )

  # Fallback: just display as plain text
  clean_args <- clean_text(arguments_content)
  return(paste0("## Arguments\n\n", clean_args, "\n\n"))
}
