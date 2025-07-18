#' Formats the rd file contents to markdown
#'
#' @param content rd_file contents from extract_rd_content
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

  # Get all section names from content, excluding those in skip_sections
  all_sections <- names(content)
  sections_to_process <- all_sections[!all_sections %in% skip_sections]
  
  # Process all sections not in skip_sections
  for (section_name in sections_to_process) {
    if (is.null(content[[section_name]])) {
      next
    }

    section_content <- content[[section_name]]

    # Format section based on type
    if (section_name == "arguments") {
      md_parts <- c(md_parts, format_arguments_section(section_content))
    } else if (section_name == "format") {
      md_parts <- c(md_parts, format_format_section(section_content))
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
        if (nrow(args_df) <= 50) {
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

#' Format format section with describe blocks as tables
#'
#' @param format_content raw format content from Rd file
#'
#' @return formatted markdown string
format_format_section <- function(format_content) {
  if (is.null(format_content) || nchar(trimws(format_content)) == 0) {
    return("")
  }
  
  clean_content <- clean_text(format_content)
  
  # Try to parse the processed format content into a table
  tryCatch({
    # Split into lines
    lines <- unlist(strsplit(clean_content, "\n"))
    lines <- trimws(lines)
    lines <- lines[nchar(lines) > 0]  # Remove empty lines
    
    # Look for lines that appear to be column definitions
    # Pattern: starts with word(s) followed by description
    column_lines <- c()
    header_lines <- c()
    
    for (i in seq_along(lines)) {
      line <- lines[i]
      
      # Check if line looks like a column definition
      # Heuristic: starts with uppercase letters/numbers, followed by space and description
      # BUT exclude dataset description lines
      if (grepl("^[A-Z][A-Z0-9_]*\\s+", line) && 
          !grepl("data frame|tibble|rows|columns|x\\s+\\d", line, ignore.case = TRUE)) {
        column_lines <- c(column_lines, line)
      } else {
        # Probably header/description text
        header_lines <- c(header_lines, line)
      }
    }
    
    # If we found column-like lines, try to parse them
    if (length(column_lines) >= 2) {  # At least 2 columns to make a table worthwhile
      items_df <- parse_format_lines(column_lines)
      
      if (nrow(items_df) > 0) {
        # Create table
        table_content <- paste0(
          "| Column | Description |\n",
          "|--------|-------------|\n",
          paste(
            apply(items_df, 1, function(row) {
              name <- clean_text(row[1])
              desc <- clean_text(row[2])
              paste0("| `", name, "` | ", desc, " |")
            }),
            collapse = "\n"
          )
        )
        
        # Combine header text with table
        result <- "## Format\n\n"
        if (length(header_lines) > 0) {
          result <- paste0(result, paste(header_lines, collapse = "\n\n"), "\n\n")
        }
        result <- paste0(result, table_content, "\n\n")
        return(result)
      }
    }
  }, error = function(e) {
    # Fall through to plain text format
  })
  
  # Fallback: display as plain text
  return(paste0("## Format\n\n", clean_content, "\n\n"))
}

#' Parse format lines into column definitions
#'
#' @param format_lines vector of lines that look like column definitions
#'
#' @return data frame with name and description columns
parse_format_lines <- function(format_lines) {
  items_list <- list()
  
  for (i in seq_along(format_lines)) {
    line <- trimws(format_lines[i])
    
    # Split on first sequence of spaces to separate name from description
    # Pattern: capture word characters/underscores, then spaces, then rest
    match_result <- regexec("^([A-Z][A-Z0-9_]*)\\s+(.+)$", line)
    captures <- regmatches(line, match_result)[[1]]
    
    if (length(captures) >= 3) {
      name <- trimws(captures[2])
      description <- trimws(captures[3])
      items_list[[i]] <- c(name = name, description = description)
    }
  }
  
  if (length(items_list) > 0) {
    items_df <- as.data.frame(
      do.call(rbind, items_list),
      stringsAsFactors = FALSE
    )
    names(items_df) <- c("name", "description")
    return(items_df)
  }
  
  return(data.frame(name = character(0), description = character(0), stringsAsFactors = FALSE))
}
