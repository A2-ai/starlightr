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
    cli::cli_alert_success("All exported functions are covered by configuration")
  } else {
    cli::cli_alert_warning("{length(missing)} exported function{?s} not in config:")
    for (fn in sort(missing)) {
      cli::cli_bullets(c("*" = "{.fn {fn}}"))
    }
  }

  if (length(config_only) > 0) {
    cli::cli_alert_info("{length(config_only)} config reference{?s} don't match exports:")
    for (ref in sort(config_only)) {
      cli::cli_bullets(c("!" = "{.val {ref}}"))
    }
  }

  # Check internal links for trailing slash, expected prefix, and valid targets
  link_entries <- find_link_entries(config)
  link_issues <- 0
  internal_link_count <- 0

  if (length(link_entries) > 0) {
    for (entry in link_entries) {
      link <- entry$link
      context <- entry$context

      if (!is_internal_link(link)) {
        next
      }

      internal_link_count <- internal_link_count + 1

      if (!startsWith(link, "./")) {
        cli::cli_warn("Link does not start with './': {.val {link}} ({context})")
        link_issues <- link_issues + 1
      }

      if (link_needs_trailing_slash(link)) {
        cli::cli_warn("Link does not end with '/': {.val {link}} ({context})")
        link_issues <- link_issues + 1
      }

      # Validate link targets (returns TRUE if issue found)
      if (validate_link_target(link, context, pkg_path, exported)) {
        link_issues <- link_issues + 1
      }
    }
  }

  if (internal_link_count > 0 && link_issues == 0) {
    cli::cli_alert_success("All {internal_link_count} internal link{?s} are valid")
  }

  invisible(list(
    missing = missing,
    covered = matched$covered,
    config_only = config_only
  ))
}

#' Parse NAMESPACE file to extract exported functions
#'
#' @param namespace_path Path to NAMESPACE file
#' @return Character vector of exported function names
#' @keywords internal
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
        exports <- c(exports, trimws(match[2]))
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
      slugs <- vapply(group$contents, function(item) {
        parse_content_item(item)$slug
      }, character(1))
      refs <- c(refs, slugs)
    }

    # Handle nested items structure
    if (!is.null(group$items)) {
      for (subgroup in group$items) {
        if (!is.null(subgroup$contents)) {
          slugs <- vapply(subgroup$contents, function(item) {
            parse_content_item(item)$slug
          }, character(1))
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
#'
#' @param ref Single reference string
#' @param exports Character vector of exported function names
#' @return Character vector of matching export names
#' @keywords internal
match_single_reference <- function(ref, exports) {
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