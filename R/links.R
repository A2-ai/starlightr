# Link utilities for starlightr

#' Determine if a link is internal
#'
#' @param link Link string
#' @return Logical indicating if link is internal
#' @keywords internal
is_internal_link <- function(link) {
  if (is.null(link) || !is.character(link) || nchar(link) == 0) {
    return(FALSE)
  }

  if (startsWith(link, "http://") || startsWith(link, "https://")) {
    return(FALSE)
  }
  if (startsWith(link, "mailto:") || startsWith(link, "tel:")) {
    return(FALSE)
  }
  if (startsWith(link, "#")) {
    return(FALSE)
  }

  TRUE
}

#' Check if an internal link needs a trailing slash
#'
#' @param link Link string
#' @return Logical indicating if link should end with '/'
#' @keywords internal
link_needs_trailing_slash <- function(link) {
  path <- sub("[?#].*$", "", link)
  nchar(path) > 0 && !grepl("/$", path)
}

#' Normalize local links to lowercase for articles/reference paths
#'
#' @param link Link string
#' @param warn Whether to warn when link is normalized
#' @return Normalized link string
#' @keywords internal
normalize_local_link <- function(link, warn = TRUE) {
  if (is.null(link) || !is.character(link)) {
    return(link)
  }

  # Only normalize local links starting with ./
  if (!startsWith(link, "./")) {
    return(link)
  }

  # Normalize ./articles/X/ and ./reference/X/ to lowercase
  if (grepl("^\\./articles/", link) || grepl("^\\./reference/", link)) {
    # Preserve any query string or hash
    parts <- strsplit(link, "[?#]")[[1]]
    path <- parts[1]
    suffix <- if (length(parts) > 1) substring(link, nchar(path) + 1) else ""

    # Lowercase the path
    normalized_path <- tolower(path)

    if (warn && normalized_path != path) {
      cli::cli_alert_info("Link normalized to lowercase: {.val {paste0(normalized_path, suffix)}}")
    }

    return(paste0(normalized_path, suffix))
  }

  link
}

#' Recursively find link fields in config for audits
#'
#' @param config Parsed YAML configuration list
#' @return List of link entries (link + context)
#' @keywords internal
find_link_entries <- function(config) {
  entries <- list()

  add_link <- function(link, context) {
    if (!is.null(link) && is.character(link) && nchar(link) > 0) {
      entries[[length(entries) + 1]] <<- list(link = link, context = context)
    }
  }

  visit <- function(node, path = character()) {
    if (is.list(node)) {
      node_names <- names(node)
      for (i in seq_along(node)) {
        name <- if (!is.null(node_names) && nzchar(node_names[i])) node_names[i] else as.character(i)
        child <- node[[i]]
        child_path <- c(path, name)

        if (!is.null(node_names) && node_names[i] %in% c("href", "link")) {
          add_link(child, paste(child_path, collapse = "."))
        }

        visit(child, child_path)
      }
    }
  }

  visit(config, character())
  entries
}

#' Validate that an internal link points to existing content
#'
#' @param link Link string
#' @param context Context string for error messages
#' @param pkg_path Path to package directory
#' @param exported Character vector of exported function names
#' @return TRUE if an issue was found, FALSE otherwise
#' @keywords internal
validate_link_target <- function(link, context, pkg_path, exported) {
  # Only validate links starting with ./
  if (!startsWith(link, "./")) {
    return(FALSE)
  }

  # Strip ./ prefix and trailing slash
  path <- sub("^\\./", "", link)
  path <- sub("/$", "", path)

  # Check articles links
  if (startsWith(path, "articles/")) {
    article_name <- sub("^articles/", "", path)
    if (nchar(article_name) > 0) {
      # README is special - it's processed from package root
      if (tolower(article_name) == "readme") {
        readme_path <- file.path(pkg_path, "README.Rmd")
        if (!file.exists(readme_path)) {
          cli::cli_warn("Article link target not found: {.val {link}} - no {.file README.Rmd} ({context})")
          return(TRUE)
        }
      } else {
        # Check for vignette file
        vignette_path <- file.path(pkg_path, "vignettes", paste0(article_name, ".Rmd"))
        if (!file.exists(vignette_path)) {
          cli::cli_warn("Article link target not found: {.val {link}} - no vignette {.file vignettes/{article_name}.Rmd} ({context})")
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }

  # Check reference links
  if (startsWith(path, "reference/")) {
    fn_name <- sub("^reference/", "", path)
    if (nchar(fn_name) > 0) {
      # Check if function is exported (case-insensitive)
      if (!any(tolower(exported) == tolower(fn_name))) {
        cli::cli_warn("Reference link target not found: {.val {link}} - no exported function {.fn {fn_name}} ({context})")
        return(TRUE)
      }
    }
    return(FALSE)
  }

  FALSE
}
