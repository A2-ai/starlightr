# Sidebar generation for starlightr

#' Generate sidebar configuration for Starlight
#'
#' @param config Configuration list from YAML
#' @param output_path Path to output directory (needed for pattern matching)
#' @param pkg_name Package name (for Rd database lookups)
#' @return JavaScript sidebar configuration as string
#' @keywords internal
generate_sidebar_config <- function(config, output_path = NULL, pkg_name = NULL) {
  sidebar_parts <- c()

  # Handle articles section
  if (!is.null(config$sidebar$articles)) {
    articles_items <- c()
    for (group in config$sidebar$articles) {
      if (!is.null(group$label) && !is.null(group$contents)) {
        group_items <- c()
        for (content in group$contents) {
          # Convert content to slug format
          slug <- paste0("articles/", content)
          # Escape quotes for JavaScript
          escaped_content <- gsub('"', '\\"', content, fixed = TRUE)
          escaped_slug <- gsub('"', '\\"', slug, fixed = TRUE)
          group_items <- c(group_items, sprintf('{ label: "%s", slug: "%s" }', escaped_content, escaped_slug))
        }
        # Handle collapsed field
        collapsed_attr <- ""
        if (!is.null(group$collapsed) && group$collapsed) {
          collapsed_attr <- ",\n          collapsed: true"
        }

        group_js <- sprintf('{\n          label: "%s"%s,\n          items: [\n            %s\n          ]\n        }',
                           group$label, collapsed_attr, paste(group_items, collapse = ",\n            "))
        articles_items <- c(articles_items, group_js)
      }
    }
    articles_section <- sprintf('{\n      label: "Articles",\n      items: [\n        %s\n      ]\n    }',
                               paste(articles_items, collapse = ",\n        "))
    sidebar_parts <- c(sidebar_parts, articles_section)
  }
  # No else - if articles not in config, don't add to sidebar

  # Handle reference section
  if (!is.null(config$sidebar$reference)) {
    reference_items <- c()
    for (group in config$sidebar$reference) {
      # Handle bare string - direct link without group wrapper
      if (is.character(group) && length(group) == 1) {
        content <- group
        doc_file <- find_function_doc_file(content, pkg_name)
        if (is.null(doc_file)) {
          doc_file <- content
        }
        slug <- paste0("reference/", doc_file)
        escaped_content <- gsub('"', '\\"', content, fixed = TRUE)
        escaped_slug <- gsub('"', '\\"', slug, fixed = TRUE)
        reference_items <- c(reference_items, sprintf('{ label: "%s", slug: "%s" }', escaped_content, escaped_slug))
        next
      }

      # Handle flat structure: label + contents
      if (!is.null(group$label) && !is.null(group$contents)) {
        group_items <- c()
        has_patterns <- any(grepl("\\*", group$contents))

        if (has_patterns && !is.null(output_path)) {
          # Handle pattern matching by finding actual files
          ref_dir <- file.path(output_path, "src", "content", "docs", "reference")
          if (dir.exists(ref_dir)) {
            all_files <- list.files(ref_dir, pattern = "\\.mdx?$", full.names = FALSE)
            all_files <- tools::file_path_sans_ext(all_files)

            # Match files against patterns in this group
            matched_files <- c()
            for (content in group$contents) {
              if (grepl("\\*$", content)) {
                # Convert pattern to regex (e.g., "extract_*" -> "^extract_")
                pattern <- paste0("^", gsub("\\*", "", content))
                matches <- all_files[grepl(pattern, all_files)]
                matched_files <- c(matched_files, matches)
              } else {
                # Exact match
                if (content %in% all_files) {
                  matched_files <- c(matched_files, content)
                }
              }
            }

            # Remove duplicates and sort
            matched_files <- unique(matched_files)
            matched_files <- sort(matched_files)

            # Create items for matched files
            for (file in matched_files) {
              slug <- paste0("reference/", file)
              # Escape quotes for JavaScript
              escaped_file <- gsub('"', '\\"', file, fixed = TRUE)
              escaped_slug <- gsub('"', '\\"', slug, fixed = TRUE)
              group_items <- c(group_items, sprintf('{ label: "%s", slug: "%s" }', escaped_file, escaped_slug))
            }
          }
        } else {
          # No patterns, handle exact names and selectors
          expanded_contents <- expand_pkgdown_selectors(group$contents, output_path)
          for (content in expanded_contents) {
            if (!grepl("\\*", content)) {
              # Map function to its actual documentation file
              doc_file <- find_function_doc_file(content, pkg_name)
              file_exists <- FALSE

              # Check if file exists (Rd-generated or manual)
              if (!is.null(output_path)) {
                ref_dir <- file.path(output_path, "src", "content", "docs", "reference")
                file_exists <- file.exists(file.path(ref_dir, paste0(content, ".mdx"))) ||
                               file.exists(file.path(ref_dir, paste0(content, ".md")))
              }

              # Use doc_file if found in Rd, otherwise use content name
              if (is.null(doc_file)) {
                doc_file <- content
              }

              # Warn if file doesn't exist
              if (!file_exists && is.null(find_function_doc_file(content, pkg_name))) {
                cli::cli_warn("Reference file missing: {.file {content}.mdx} - create manually or check config")
              }

              slug <- paste0("reference/", doc_file)
              escaped_content <- gsub('"', '\\"', content, fixed = TRUE)
              escaped_slug <- gsub('"', '\\"', slug, fixed = TRUE)
              group_items <- c(group_items, sprintf('{ label: "%s", slug: "%s" }', escaped_content, escaped_slug))
            }
          }
        }

        # Add the group if we have items
        if (length(group_items) > 0) {
          # Handle collapsed field
          collapsed_attr <- ""
          if (!is.null(group$collapsed) && group$collapsed) {
            collapsed_attr <- ",\n          collapsed: true"
          }

          group_js <- sprintf('{\n          label: "%s"%s,\n          items: [\n            %s\n          ]\n        }',
                             group$label, collapsed_attr, paste(group_items, collapse = ",\n            "))
          reference_items <- c(reference_items, group_js)
        }
      }
      # Handle nested structure: label + items (each item has label + contents)
      else if (!is.null(group$label) && !is.null(group$items)) {
        nested_items <- c()

        for (subgroup in group$items) {
          if (!is.null(subgroup$label) && !is.null(subgroup$contents)) {
            subgroup_items <- c()

            # Process each function in the subgroup contents
            expanded_contents <- expand_pkgdown_selectors(subgroup$contents, output_path)
            for (content in expanded_contents) {
              # Map function to its actual documentation file
              doc_file <- find_function_doc_file(content, pkg_name)
              file_exists <- FALSE

              # Check if file exists (Rd-generated or manual)
              if (!is.null(output_path)) {
                ref_dir <- file.path(output_path, "src", "content", "docs", "reference")
                file_exists <- file.exists(file.path(ref_dir, paste0(content, ".mdx"))) ||
                               file.exists(file.path(ref_dir, paste0(content, ".md")))
              }

              # Use doc_file if found in Rd, otherwise use content name
              if (is.null(doc_file)) {
                doc_file <- content
              }

              # Warn if file doesn't exist
              if (!file_exists && is.null(find_function_doc_file(content, pkg_name))) {
                cli::cli_warn("Reference file missing: {.file {content}.mdx} - create manually or check config")
              }

              slug <- paste0("reference/", doc_file)
              escaped_content <- gsub('"', '\\"', content, fixed = TRUE)
              escaped_slug <- gsub('"', '\\"', slug, fixed = TRUE)
              subgroup_items <- c(subgroup_items, sprintf('{ label: "%s", slug: "%s" }', escaped_content, escaped_slug))
            }

            # Handle collapsed field for subgroups
            subcollapsed_attr <- ""
            if (!is.null(subgroup$collapsed) && subgroup$collapsed) {
              subcollapsed_attr <- ",\n            collapsed: true"
            }

            # Create subgroup JavaScript
            subgroup_js <- sprintf('{\n            label: "%s"%s,\n            items: [\n              %s\n            ]\n          }',
                                   subgroup$label, subcollapsed_attr, paste(subgroup_items, collapse = ",\n              "))
            nested_items <- c(nested_items, subgroup_js)
          }
        }

        if (length(nested_items) > 0) {
          # Handle collapsed field for main group
          collapsed_attr <- ""
          if (!is.null(group$collapsed) && group$collapsed) {
            collapsed_attr <- ",\n          collapsed: true"
          }

          group_js <- sprintf('{\n          label: "%s"%s,\n          items: [\n            %s\n          ]\n        }',
                             group$label, collapsed_attr, paste(nested_items, collapse = ",\n            "))
          reference_items <- c(reference_items, group_js)
        }
      }
    }

    if (length(reference_items) > 0) {
      reference_section <- sprintf('{\n      label: "Reference",\n      items: [\n        %s\n      ]\n    }',
                                  paste(reference_items, collapse = ",\n        "))
      sidebar_parts <- c(sidebar_parts, reference_section)
    } else {
      # Fallback to autogenerate if no items
      sidebar_parts <- c(sidebar_parts, '{\n      label: "Reference",\n      autogenerate: { directory: "reference" }\n    }')
    }
  } else {
    # Fallback to autogenerate
    sidebar_parts <- c(sidebar_parts, '{\n      label: "Reference",\n      autogenerate: { directory: "reference" }\n    }')
  }

  # Handle news/changelog section
  if (!is.null(config$sidebar$news)) {
    news_label <- config$sidebar$news$label %||% "Changelog"
    news_section <- sprintf('{ label: "%s", slug: "news" }', news_label)
    sidebar_parts <- c(sidebar_parts, news_section)
  }

  # Combine all parts
  sidebar_config <- sprintf('[\n    %s\n  ]', paste(sidebar_parts, collapse = ",\n    "))
  return(sidebar_config)
}

#' Expand pkgdown selector functions to actual function names
#'
#' @param contents Vector of function names and selectors
#' @param output_path Path to output directory to find available functions
#'
#' @return Vector with selectors expanded to actual function names
#' @keywords internal
expand_pkgdown_selectors <- function(contents, output_path = NULL) {
  if (is.null(contents) || length(contents) == 0) {
    return(character(0))
  }

  # Get all available functions from the reference directory
  available_functions <- character(0)
  if (!is.null(output_path)) {
    ref_dir <- file.path(output_path, "src", "content", "docs", "reference")
    if (dir.exists(ref_dir)) {
      ref_files <- list.files(ref_dir, pattern = "\\.mdx?$", full.names = FALSE)
      available_functions <- tools::file_path_sans_ext(ref_files)
    }
  }

  expanded <- character(0)

  for (content in contents) {
    # Check if this is a selector function
    if (is_pkgdown_selector(content)) {
      # Expand the selector
      matches <- expand_selector(content, available_functions)
      expanded <- c(expanded, matches)
    } else {
      # Keep as-is
      expanded <- c(expanded, content)
    }
  }

  # Remove duplicates and return
  unique(expanded)
}

#' Check if a string is a pkgdown selector function
#'
#' @param content String to check
#'
#' @return Logical indicating if it's a selector
#' @keywords internal
is_pkgdown_selector <- function(content) {
  grepl("^(starts_with|ends_with|contains|matches)\\s*\\(", content)
}

#' Expand a single selector to matching function names
#'
#' @param selector Selector string like 'ends_with("_at")'
#' @param available_functions Vector of available function names
#'
#' @return Vector of matching function names
#' @keywords internal
expand_selector <- function(selector, available_functions) {
  if (length(available_functions) == 0) {
    return(character(0))
  }

  # Extract the selector type and pattern
  if (grepl("^starts_with\\s*\\(", selector)) {
    pattern <- extract_quoted_pattern(selector)
    if (!is.null(pattern)) {
      regex <- paste0("^", escape_regex(pattern))
      return(available_functions[grepl(regex, available_functions)])
    }
  } else if (grepl("^ends_with\\s*\\(", selector)) {
    pattern <- extract_quoted_pattern(selector)
    if (!is.null(pattern)) {
      regex <- paste0(escape_regex(pattern), "$")
      return(available_functions[grepl(regex, available_functions)])
    }
  } else if (grepl("^contains\\s*\\(", selector)) {
    pattern <- extract_quoted_pattern(selector)
    if (!is.null(pattern)) {
      regex <- escape_regex(pattern)
      return(available_functions[grepl(regex, available_functions)])
    }
  } else if (grepl("^matches\\s*\\(", selector)) {
    pattern <- extract_quoted_pattern(selector)
    if (!is.null(pattern)) {
      # For matches, use the pattern as-is (it's already a regex)
      return(available_functions[grepl(pattern, available_functions)])
    }
  }

  # If we can't parse the selector, return empty
  character(0)
}

#' Extract the quoted pattern from a selector function
#'
#' @param selector Selector string like 'ends_with("_at")'
#'
#' @return The extracted pattern or NULL if not found
#' @keywords internal
extract_quoted_pattern <- function(selector) {
  # Match patterns like: ends_with("pattern") or ends_with('pattern')
  matches <- regexec('\\(\\s*["\']([^"\']+)["\']\\s*\\)', selector)
  if (matches[[1]][1] != -1) {
    captures <- regmatches(selector, matches)[[1]]
    if (length(captures) >= 2) {
      return(captures[2])
    }
  }
  return(NULL)
}

#' Escape special regex characters in a pattern
#'
#' @param pattern String to escape
#'
#' @return Escaped string
#' @keywords internal
escape_regex <- function(pattern) {
  gsub("([.^$*+?{}\\[\\]\\(\\)|\\\\])", "\\\\\\1", pattern)
}

#' Find the actual documentation file for a function using Rd database
#'
#' Handles @rdname cases where functions are documented in different files
#'
#' @param function_name Name of the function to find
#' @param pkg_name Package name (for accessing Rd database)
#'
#' @return The base name of the Rd file containing the function's documentation, or NULL
#' @keywords internal
find_function_doc_file <- function(function_name, pkg_name) {
  if (is.null(function_name) || is.null(pkg_name) || nchar(function_name) == 0) {
    return(NULL)
  }

  tryCatch({
    # Get the Rd database for the package
    rd_db <- tools::Rd_db(pkg_name)

    # Search through all Rd files for the function
    for (rd_file_name in names(rd_db)) {
      rd_obj <- rd_db[[rd_file_name]]

      # Check if this Rd object documents the function we're looking for
      if (rd_contains_function(rd_obj, function_name)) {
        # Return the base name without .Rd extension
        return(tools::file_path_sans_ext(rd_file_name))
      }
    }

    return(NULL)
  }, error = function(e) {
    # If we can't access Rd database, return NULL
    return(NULL)
  })
}

#' Check if an Rd object contains documentation for a specific function
#'
#' @param rd_obj Rd object from tools::Rd_db
#' @param function_name Function name to look for
#'
#' @return Logical indicating if the function is documented in this Rd object
#' @keywords internal
rd_contains_function <- function(rd_obj, function_name) {
  # Check the \name section first
  name_section <- get_rd_section(rd_obj, "name")
  if (!is.null(name_section)) {
    name_text <- trimws(paste(unlist(name_section), collapse = ""))
    if (name_text == function_name) {
      return(TRUE)
    }
  }

  # Check \alias sections (functions documented via @rdname appear as aliases)
  aliases <- character()
  for (element in rd_obj) {
    el_tag <- attr(element, "Rd_tag")
    if (!is.null(el_tag) && el_tag == "\\alias") {
      alias_content <- paste(unlist(element), collapse = "")
      alias_content <- trimws(alias_content)
      aliases <- c(aliases, alias_content)
    }
  }

  # Check if our function is in the aliases
  return(function_name %in% aliases)
}
