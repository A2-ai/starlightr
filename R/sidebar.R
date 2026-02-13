# Sidebar generation for starlightr

#' Create a single sidebar item JSON string
#'
#' @param label Display label
#' @param slug URL slug
#' @return JSON string for sidebar item
#' @keywords internal
make_sidebar_item <- function(label, slug) {

  escaped_label <- gsub('"', '\\"', label, fixed = TRUE)
  escaped_slug <- gsub('"', '\\"', slug, fixed = TRUE)
  sprintf('{ label: "%s", slug: "%s" }', escaped_label, escaped_slug)
}

#' Parse a content item which can be a string or a list with slug/label
#'
#' @param content Content item (string or list)
#' @return List with slug and label
#' @keywords internal
parse_content_item <- function(content) {
  if (is.list(content)) {
    slug <- content$slug %||% content$label %||% ""
    label <- content$label %||% content$slug %||% ""
    list(slug = slug, label = label)
  } else {
    list(slug = content, label = content)
  }
}

#' Create a sidebar group JSON string
#'
#' @param label Group label
#' @param items Vector of item JSON strings
#' @param collapsed Whether group is collapsed
#' @param indent Base indentation level
#' @return JSON string for sidebar group
#' @keywords internal
make_sidebar_group <- function(label, items, collapsed = FALSE, indent = 10) {
  label <- gsub("\\", "\\\\", label, fixed = TRUE)
  label <- gsub('"', '\\"', label, fixed = TRUE)
  collapsed_attr <- if (collapsed) ",\n          collapsed: true" else ""
  item_sep <- paste0(",\n", strrep(" ", indent + 2))

  sprintf('{\n          label: "%s"%s,\n          items: [\n            %s\n          ]\n        }',
          label, collapsed_attr, paste(items, collapse = item_sep))
}

#' Resolve a reference content item to a sidebar item
#'
#' Handles doc file lookup, file existence checks, and warnings.
#'
#' @param content Function/content name (used for slug)
#' @param output_path Output path for file existence checks
#' @param pkg_name Package name for Rd lookups
#' @param warn Whether to warn about missing files
#' @param label Optional display label (defaults to content)
#' @return Sidebar item JSON string or NULL if should skip
#' @keywords internal
resolve_reference_item <- function(content, output_path, pkg_name, warn = TRUE, label = NULL, rd_db = NULL) {
  # Map function to its actual documentation file
  doc_file <- find_function_doc_file(content, pkg_name, rd_db = rd_db)

  # Check if file exists
  file_exists <- FALSE
  if (!is.null(output_path)) {
    ref_dir <- file.path(output_path, "src", "content", "docs", "reference")
    content_lower <- tolower(content)
    file_exists <- file.exists(file.path(ref_dir, paste0(content_lower, ".mdx"))) ||
                   file.exists(file.path(ref_dir, paste0(content_lower, ".md")))
  }

  # Use doc_file if found, otherwise use content name
  if (is.null(doc_file)) {
    doc_file <- content
  }

  # Warn if file doesn't exist
  if (warn && !file_exists && is.null(doc_file)) {
    cli::cli_warn("Reference file missing: {.file {content}.mdx} - create manually or check config")
  }

  # Use provided label or default to content name
  display_label <- label %||% content

  # Sanitize slug: lowercase and replace dots with hyphens (Astro requirement)
  slug_name <- gsub(".", "-", tolower(doc_file), fixed = TRUE)
  slug <- paste0("reference/", slug_name)
  make_sidebar_item(display_label, slug)
}

#' Get available reference files
#'
#' @param output_path Output path
#' @return Character vector of file names without extension
#' @keywords internal
get_available_ref_files <- function(output_path) {
  if (is.null(output_path)) return(character(0))

  ref_dir <- file.path(output_path, "src", "content", "docs", "reference")
  if (!dir.exists(ref_dir)) return(character(0))

  files <- list.files(ref_dir, pattern = "\\.mdx?$", full.names = FALSE)
  tools::file_path_sans_ext(files)
}

#' Expand glob patterns to matching files
#'
#' @param patterns Vector of patterns (may include * wildcards)
#' @param available_files Vector of available file names
#' @return Vector of matched file names
#' @keywords internal
expand_glob_patterns <- function(patterns, available_files) {
  matched <- character(0)

  for (pattern in patterns) {
    if (grepl("\\*$", pattern)) {
      # Convert glob to regex
      regex <- paste0("^", gsub("\\*", "", pattern))
      matches <- available_files[grepl(regex, available_files)]
      matched <- c(matched, matches)
    } else if (pattern %in% available_files) {
      matched <- c(matched, pattern)
    }
  }

  unique(sort(matched))
}

#' Generate sidebar configuration for Starlight
#'
#' @param config Configuration list from YAML
#' @param output_path Path to output directory (needed for pattern matching)
#' @param pkg_name Package name (for Rd database lookups)
#' @return JavaScript sidebar configuration as string
#' @keywords internal
generate_sidebar_config <- function(config, output_path = NULL, pkg_name = NULL) {
  rd_db <- if (!is.null(pkg_name)) {
    tryCatch(tools::Rd_db(pkg_name), error = function(e) NULL)
  } else {
    NULL
  }
  resolve_ref <- function(content, warn = TRUE, label = NULL) {
    resolve_reference_item(
      content = content,
      output_path = output_path,
      pkg_name = pkg_name,
      warn = warn,
      label = label,
      rd_db = rd_db
    )
  }

  sidebar_parts <- c()

  # Handle articles section
  if (!is.null(config$sidebar$articles)) {
    articles_items <- c()
    for (group in config$sidebar$articles) {
      if (!is.null(group$label) && !is.null(group$contents)) {
        group_items <- vapply(group$contents, function(content) {
          parsed <- parse_content_item(content)
          slug <- paste0("articles/", gsub(".", "-", tolower(parsed$slug), fixed = TRUE))
          make_sidebar_item(parsed$label, slug)
        }, character(1))

        group_js <- make_sidebar_group(group$label, group_items, group$collapsed %||% FALSE)
        articles_items <- c(articles_items, group_js)
      }
    }

    articles_section <- sprintf('{\n      label: "Articles",\n      items: [\n        %s\n      ]\n    }',
                               paste(articles_items, collapse = ",\n        "))
    sidebar_parts <- c(sidebar_parts, articles_section)
  }

  # Handle reference section
  if (!is.null(config$sidebar$reference)) {
    reference_items <- c()
    available_files <- get_available_ref_files(output_path)

    for (group in config$sidebar$reference) {
      # Handle bare string - direct link without group wrapper
      if (is.character(group) && length(group) == 1) {
        item <- resolve_ref(group, warn = FALSE)
        reference_items <- c(reference_items, item)
        next
      }

      # Handle flat structure: label + contents
      if (!is.null(group$label) && !is.null(group$contents)) {
        group_items <- c()
        # Extract slugs for pattern detection (handles both string and list items)
        content_slugs <- vapply(group$contents, function(c) {
          parse_content_item(c)$slug
        }, character(1))
        has_patterns <- any(grepl("\\*", content_slugs))

        if (has_patterns && length(available_files) > 0) {
          # Handle pattern matching
          matched_files <- expand_glob_patterns(content_slugs, available_files)
          group_items <- vapply(matched_files, function(file) {
            slug <- paste0("reference/", gsub(".", "-", tolower(file), fixed = TRUE))
            make_sidebar_item(file, slug)
          }, character(1))
        } else {
          # Handle exact names and selectors
          for (content in group$contents) {
            parsed <- parse_content_item(content)
            if (!grepl("\\*", parsed$slug) && !is_pkgdown_selector(parsed$slug)) {
              item <- resolve_ref(parsed$slug, label = parsed$label)
              group_items <- c(group_items, item)
            } else if (is_pkgdown_selector(parsed$slug)) {
              # Expand selector
              expanded <- expand_selector(parsed$slug, available_files)
              for (fn in expanded) {
                item <- resolve_ref(fn)
                group_items <- c(group_items, item)
              }
            }
          }
        }

        if (length(group_items) > 0) {
          group_js <- make_sidebar_group(group$label, group_items, group$collapsed %||% FALSE)
          reference_items <- c(reference_items, group_js)
        }
      }
      # Handle nested structure: label + items (each item has label + contents)
      else if (!is.null(group$label) && !is.null(group$items)) {
        nested_items <- c()

        for (subgroup in group$items) {
          if (!is.null(subgroup$label) && !is.null(subgroup$contents)) {
            expanded_contents <- expand_pkgdown_selectors(subgroup$contents, output_path)
            subgroup_items <- vapply(expanded_contents, function(content) {
              resolve_ref(content)
            }, character(1))

            subgroup_js <- make_sidebar_group(subgroup$label, subgroup_items, subgroup$collapsed %||% FALSE)
            nested_items <- c(nested_items, subgroup_js)
          }
        }

        if (length(nested_items) > 0) {
          group_js <- make_sidebar_group(group$label, nested_items, group$collapsed %||% FALSE)
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
    news_section <- make_sidebar_item(news_label, "news")
    sidebar_parts <- c(sidebar_parts, news_section)
  }

  # Combine all parts
  sprintf('[\n    %s\n  ]', paste(sidebar_parts, collapse = ",\n    "))
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

  available_functions <- get_available_ref_files(output_path)

  expanded <- character(0)
  for (content in contents) {
    if (is_pkgdown_selector(content)) {
      matches <- expand_selector(content, available_functions)
      expanded <- c(expanded, matches)
    } else {
      expanded <- c(expanded, content)
    }
  }

  unique(expanded)
}

#' Check if a string is a pkgdown selector function
#'
#' @param content String to check
#' @return Logical indicating if it's a selector
#' @keywords internal
is_pkgdown_selector <- function(content) {
  grepl("^(starts_with|ends_with|contains|matches)\\s*\\(", content)
}

#' Expand a single selector to matching function names
#'
#' @param selector Selector string like 'ends_with("_at")'
#' @param available_functions Vector of available function names
#' @return Vector of matching function names
#' @keywords internal
expand_selector <- function(selector, available_functions) {
  if (length(available_functions) == 0) {
    return(character(0))
  }

  pattern <- extract_quoted_pattern(selector)
  if (is.null(pattern)) return(character(0))

  if (grepl("^starts_with\\s*\\(", selector)) {
    regex <- paste0("^", escape_regex(pattern))
  } else if (grepl("^ends_with\\s*\\(", selector)) {
    regex <- paste0(escape_regex(pattern), "$")
  } else if (grepl("^contains\\s*\\(", selector)) {
    regex <- escape_regex(pattern)
  } else if (grepl("^matches\\s*\\(", selector)) {
    regex <- pattern  # Already a regex
  } else {
    return(character(0))
  }

  available_functions[grepl(regex, available_functions)]
}

#' Extract the quoted pattern from a selector function
#'
#' @param selector Selector string like 'ends_with("_at")'
#' @return The extracted pattern or NULL if not found
#' @keywords internal
extract_quoted_pattern <- function(selector) {
  matches <- regexec('\\(\\s*["\']([^"\']+)["\']\\s*\\)', selector)
  if (matches[[1]][1] != -1) {
    captures <- regmatches(selector, matches)[[1]]
    if (length(captures) >= 2) {
      return(captures[2])
    }
  }
  NULL
}

#' Escape special regex characters in a pattern
#'
#' @param pattern String to escape
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
#' @return The base name of the Rd file containing the function's documentation, or NULL
#' @keywords internal
find_function_doc_file <- function(function_name, pkg_name, rd_db = NULL) {
  if (is.null(function_name) || is.null(pkg_name) || nchar(function_name) == 0) {
    return(NULL)
  }

  tryCatch({
    if (is.null(rd_db)) {
      rd_db <- tools::Rd_db(pkg_name)
    }

    for (rd_file_name in names(rd_db)) {
      rd_obj <- rd_db[[rd_file_name]]
      if (rd_contains_function(rd_obj, function_name)) {
        return(tools::file_path_sans_ext(rd_file_name))
      }
    }

    NULL
  }, error = function(e) NULL)
}

#' Check if an Rd object contains documentation for a specific function
#'
#' @param rd_obj Rd object from tools::Rd_db
#' @param function_name Function name to look for
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
  for (element in rd_obj) {
    el_tag <- attr(element, "Rd_tag")
    if (!is.null(el_tag) && el_tag == "\\alias") {
      alias_content <- trimws(paste(unlist(element), collapse = ""))
      if (alias_content == function_name) {
        return(TRUE)
      }
    }
  }

  FALSE
}
