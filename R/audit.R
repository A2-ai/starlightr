# Configuration audit functions for starlightr

#' Audit configuration against NAMESPACE exports
#'
#' Compares exported functions in NAMESPACE against functions referenced in
#' the _starlightr.toml configuration file. Reports any exported functions
#' that are not covered by the sidebar reference configuration.
#'
#' @param pkg Path to package directory, defaults to current directory
#' @param config_file Path to _starlightr.toml configuration file
#'
#' @return Invisibly returns a list with:
#'   - missing: character vector of exported functions not in config
#'   - covered: character vector of exported functions covered by config
#'   - config_only: character vector of config references that don't match exports
#' @export
#'
#' @examples \dontrun{
#' # Audit current package
#' audit_config()
#'
#' # Audit specific package
#' audit_config("/path/to/package")
#' }
audit_config <- function(pkg = ".", config_file = "_starlightr.toml") {
  pkg_path <- normalizePath(pkg, mustWork = TRUE)

  # Parse NAMESPACE to get exported functions
  namespace_path <- file.path(pkg_path, "NAMESPACE")
  if (!file.exists(namespace_path)) {
    cli::cli_abort("NAMESPACE file not found at {.path {namespace_path}}")
  }

  exported <- parse_namespace_exports(namespace_path)

  # Read configuration using shared helper
  config_path <- file.path(pkg_path, config_file)
  config <- read_config_toml(config_path)

  # Extract all function references from sidebar.reference
  config_refs <- extract_config_references(config)

  # Match config references against exports
  matched <- match_config_to_exports(config_refs, exported)

  # Find missing (exported but not in config)
  missing <- setdiff(exported, matched$covered)

  # Find config references that don't match any export
  config_only <- matched$unmatched

  # Report results
  cli::cli_h1("Configuration Audit")
  cli::cli_alert_info("Package: {.pkg {basename(pkg_path)}}")
  cli::cli_alert_info("Exported functions: {length(exported)}")
  cli::cli_alert_info("Config references: {length(config_refs)}")

  if (length(missing) == 0) {
    cli::cli_alert_success(
      "All exported functions are covered by configuration"
    )
  } else {
    cli::cli_alert_warning(
      "{length(missing)} exported function{?s} not in config:"
    )
    for (fn in sort(missing)) {
      cli::cli_bullets(c("*" = "{.fn {fn}}"))
    }
  }

  # A reference that isn't an export may still be a real topic: datasets and
  # @rdname package pages are documented but never exported.
  include_internal <- config$reference$include_internal %||% FALSE
  buckets <- classify_unmatched_references(config_only, pkg_path)

  if (length(buckets$topics) > 0) {
    cli::cli_alert_success(
      "{length(buckets$topics)} package or data topic{?s} documented but not exported"
    )
  }

  if (length(buckets$internal) > 0) {
    if (include_internal) {
      cli::cli_alert_success(
        "{length(buckets$internal)} internal topic{?s} allowed by {.code reference.include_internal}"
      )
    } else {
      cli::cli_alert_warning(
        "{length(buckets$internal)} internal topic{?s} in config but {.code reference.include_internal} is false:"
      )
      for (ref in sort(buckets$internal)) {
        cli::cli_bullets(c("!" = "{.val {ref}}"))
      }
    }
  }

  if (length(buckets$unknown) > 0) {
    cli::cli_alert_warning(
      "{length(buckets$unknown)} config reference{?s} match no documented topic:"
    )
    for (ref in sort(buckets$unknown)) {
      cli::cli_bullets(c("!" = "{.val {ref}}"))
    }
  }

  # Validate article slugs against vignette files
  article_result <- validate_article_slugs(config, pkg_path)

  if (article_result$valid_count > 0) {
    cli::cli_alert_success(
      "{article_result$valid_count} article slug{?s} validated"
    )
  }

  if (length(article_result$missing) > 0) {
    cli::cli_alert_warning(
      "{length(article_result$missing)} article{?s} in config not found:"
    )
    for (slug in article_result$missing) {
      if (tolower(slug) == "readme") {
        cli::cli_bullets(c("!" = "{.val {slug}} - no {.file README.Rmd}"))
      } else {
        cli::cli_bullets(c(
          "!" = "{.val {slug}} - no {.file vignettes/{slug}.Rmd}"
        ))
      }
    }
  }

  # Check internal links for trailing slash, expected prefix, and valid targets
  link_entries <- find_link_entries(config)
  link_issues <- 0
  internal_link_count <- 0
  external_links <- list()
  non_relative_links <- list()
  external_urls <- list()

  if (length(link_entries) > 0) {
    for (entry in link_entries) {
      link <- entry$link
      context <- entry$context

      # Collect external URLs (http/https) for informational reporting
      if (startsWith(link, "http://") || startsWith(link, "https://")) {
        external_urls[[length(external_urls) + 1]] <- list(
          link = link,
          context = context
        )
        next
      }

      if (!is_internal_link(link)) {
        next
      }

      internal_link_count <- internal_link_count + 1

      # Collect non-relative links (don't start with ./)
      if (!startsWith(link, "./")) {
        non_relative_links[[length(non_relative_links) + 1]] <- list(
          link = link,
          context = context
        )
        next
      }

      if (link_needs_trailing_slash(link)) {
        cli::cli_warn("Link missing trailing slash: {.val {link}} ({context})")
        link_issues <- link_issues + 1
      }

      # Check if this is an external doc link (not articles/reference)
      if (is_external_doc_link(link)) {
        external_links[[length(external_links) + 1]] <- list(
          link = link,
          context = context
        )
        next
      }

      # Validate link targets (returns TRUE if issue found)
      if (validate_link_target(link, context, pkg_path, exported)) {
        link_issues <- link_issues + 1
      }
    }
  }

  validated_count <- internal_link_count -
    length(external_links) -
    length(non_relative_links)
  if (validated_count > 0 && link_issues == 0) {
    cli::cli_alert_success("{validated_count} internal link{?s} validated")
  }

  # Report non-relative links
  if (length(non_relative_links) > 0) {
    cli::cli_alert_warning(
      "{length(non_relative_links)} non-relative link{?s} found (should start with './'):"
    )
    for (nrl in non_relative_links) {
      cli::cli_bullets(c("!" = "{.val {nrl$link}} ({nrl$context})"))
    }
  }

  # Report external links (not validated)
  if (length(external_links) > 0) {
    cli::cli_alert_info(
      "{length(external_links)} external doc link{?s} found (not validated):"
    )
    for (ext in external_links) {
      cli::cli_bullets(c("*" = "{.val {ext$link}} ({ext$context})"))
    }
  }

  # Report external URLs (http/https)
  if (length(external_urls) > 0) {
    cli::cli_alert_info("{length(external_urls)} external URL{?s} found:")
    for (ext in external_urls) {
      cli::cli_bullets(c("*" = "{.url {ext$link}} ({ext$context})"))
    }
  }

  invisible(list(
    missing = missing,
    covered = matched$covered,
    config_only = config_only
  ))
}

#' Classify config references that matched no NAMESPACE export
#'
#' A reference can still be valid: datasets and `@rdname` package topics are
#' documented but never exported, and internal topics are legitimate when
#' `reference.include_internal` is set. Anything with no Rd file behind it is
#' a typo or a deleted topic.
#'
#' @param refs Character vector of unmatched config references
#' @param pkg_path Package directory path
#' @return List with `topics`, `internal` and `unknown` character vectors
#' @keywords internal
#' @noRd
classify_unmatched_references <- function(refs, pkg_path) {
  out <- list(
    topics = character(),
    internal = character(),
    unknown = character()
  )
  if (length(refs) == 0) {
    return(out)
  }

  rd_files <- list.files(
    file.path(pkg_path, "man"),
    pattern = "\\.Rd$",
    full.names = TRUE
  )
  rd_names <- tolower(tools::file_path_sans_ext(basename(rd_files)))

  for (ref in refs) {
    idx <- match(tolower(ref), rd_names)
    if (is.na(idx)) {
      out$unknown <- c(out$unknown, ref)
    } else if (is_internal_rd(rd_files[idx])) {
      out$internal <- c(out$internal, ref)
    } else {
      out$topics <- c(out$topics, ref)
    }
  }

  out
}

#' Parse NAMESPACE file to extract exported functions
#'
#' @param namespace_path Path to NAMESPACE file
#' @return Character vector of exported function names
#' @keywords internal
#' @noRd
parse_namespace_exports <- function(namespace_path) {
  lines <- readLines(namespace_path, warn = FALSE)

  exports <- character()

  for (line in lines) {
    line <- trimws(line)

    # Skip comments and empty lines
    if (nchar(line) == 0 || startsWith(line, "#")) {
      next
    }

    # Match export(function_name)
    if (grepl("^export\\(", line)) {
      match <- regmatches(line, regexec("^export\\(([^)]+)\\)", line))[[1]]
      if (length(match) >= 2) {
        symbols <- strsplit(match[2], ",")[[1]]
        exports <- c(exports, trimws(symbols))
      }
    }

    # Match S3method(generic, class) -> generic.class
    # Also handles S3method(pkg::generic, class) by stripping the package prefix
    if (grepl("^S3method\\(", line)) {
      match <- regmatches(
        line,
        regexec("^S3method\\(([^,]+),\\s*([^)]+)\\)", line)
      )[[1]]
      if (length(match) >= 3) {
        generic <- trimws(match[2])
        class <- trimws(match[3])
        # Strip package prefix if present (e.g., base::print -> print)
        generic <- sub("^[a-zA-Z0-9.]+::", "", generic)
        exports <- c(exports, paste0(generic, ".", class))
      }
    }

    # Match exportPattern("pattern") - less common but valid
    if (grepl("^exportPattern\\(", line)) {
      # For patterns, we can't enumerate them here - would need actual function list
      # Skip for now, could be enhanced later
    }
  }

  unique(exports)
}

#' Extract all function references from config sidebar.reference
#'
#' @param config Parsed YAML configuration list
#' @return Character vector of function names and patterns
#' @keywords internal
#' @noRd
extract_config_references <- function(config) {
  refs <- character()

  if (is.null(config$sidebar$reference)) {
    return(refs)
  }

  for (group in config$sidebar$reference) {
    # Handle bare string - direct reference
    if (is.character(group) && length(group) == 1) {
      refs <- c(refs, group)
      next
    }

    if (!is.null(group$contents)) {
      slugs <- vapply(
        group$contents,
        function(item) {
          parse_content_item(item)$slug
        },
        character(1)
      )
      refs <- c(refs, slugs)
    }

    # Handle nested items structure
    if (!is.null(group$items)) {
      for (subgroup in group$items) {
        if (!is.null(subgroup$contents)) {
          slugs <- vapply(
            subgroup$contents,
            function(item) {
              parse_content_item(item)$slug
            },
            character(1)
          )
          refs <- c(refs, slugs)
        }
      }
    }
  }

  unique(refs)
}

#' Match config references against exported functions
#'
#' @param config_refs Character vector of config references (names and patterns)
#' @param exports Character vector of exported function names
#' @return List with covered (matched exports) and unmatched (config refs that don't match)
#' @keywords internal
#' @noRd
match_config_to_exports <- function(config_refs, exports) {
  covered <- character()
  unmatched <- character()

  for (ref in config_refs) {
    matched_exports <- match_single_reference(ref, exports)

    if (length(matched_exports) > 0) {
      covered <- c(covered, matched_exports)
    } else {
      unmatched <- c(unmatched, ref)
    }
  }

  list(
    covered = unique(covered),
    unmatched = unique(unmatched)
  )
}

#' Match a single config reference against exports
#'
#' Handles exact names, glob patterns (e.g., extract_*), and pkgdown selectors.
#' Matching is case-insensitive since MDX filenames are lowercased.
#' Package documentation files (ending in -package) are treated as valid.
#'
#' @param ref Single reference string
#' @param exports Character vector of exported function names
#' @return Character vector of matching export names
#' @keywords internal
#' @noRd
match_single_reference <- function(ref, exports) {
  # Package documentation files (e.g., "mypackage-package") are valid references
  # but not exports - return the ref itself to mark as "covered"
  if (grepl("-package$", ref)) {
    return(ref)
  }

  # Check for pkgdown selector functions
  if (is_pkgdown_selector(ref)) {
    return(expand_selector(ref, exports))
  }

  # Check for glob pattern (ends with *)
  if (grepl("\\*$", ref)) {
    pattern <- paste0("^", gsub("\\*", ".*", ref), "$")
    return(exports[grepl(pattern, exports, ignore.case = TRUE)])
  }

  # Case-insensitive exact match
  matches <- exports[tolower(exports) == tolower(ref)]
  if (length(matches) > 0) {
    return(matches)
  }

  character()
}
