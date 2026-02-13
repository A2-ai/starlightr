# Utility functions for starlightr

# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Check if a path is absolute
#'
#' @param path Character string representing a file path
#' @return Logical indicating if the path is absolute
#' @keywords internal
is_absolute_path <- function(path) {
  # Check for Unix-style absolute paths (starting with /)
  if (grepl("^/", path)) return(TRUE)

  # Check for Windows-style absolute paths (C:\ or \\)
  if (grepl("^[A-Za-z]:", path) || grepl("^\\\\", path)) return(TRUE)

  # Check for tilde expansion (~)
  if (grepl("^~", path)) return(TRUE)

  return(FALSE)
}

#' Get package name from DESCRIPTION file
#'
#' @param pkg_path Path to package directory
#' @return Package name as string
#' @keywords internal
get_package_name <- function(pkg_path) {
  desc_path <- file.path(pkg_path, "DESCRIPTION")

  if (!file.exists(desc_path)) {
    stop("DESCRIPTION file not found in: ", pkg_path)
  }

  desc_lines <- readLines(desc_path)
  name_line <- grep("^Package:", desc_lines, value = TRUE)

  if (length(name_line) == 0) {
    stop("Package name not found in DESCRIPTION file")
  }

  pkg_name <- trimws(sub("^Package:\\s*", "", name_line[1]))
  return(pkg_name)
}

#' Find README.Rmd in package root or inst/
#'
#' @param pkg_path Path to package directory
#' @return README path if found, otherwise NULL
#' @keywords internal
find_readme <- function(pkg_path) {
  candidates <- c(
    file.path(pkg_path, "README.Rmd"),
    file.path(pkg_path, "inst", "README.Rmd")
  )

  for (path in candidates) {
    if (file.exists(path)) {
      return(path)
    }
  }

  NULL
}

#' Extract GitHub URL from configuration
#'
#' @param config Configuration list
#' @return GitHub URL or NULL
#' @keywords internal
get_github_url <- function(config) {
  # Look for GitHub URL in navbar right section
  if (!is.null(config$navbar$right)) {
    for (item in config$navbar$right) {
      if (!is.null(item$icon) && item$icon == "github") {
        return(item$href)
      }
    }
  }
  return(NULL)
}

#' Render a whisker template from inst/templates/
#'
#' @param name Template filename (e.g. "astro.config.mjs")
#' @param data Named list of variables for whisker interpolation
#' @return Rendered template as a character string
#' @keywords internal
render_template <- function(name, data = list()) {
  template_path <- system.file("templates", name, package = "starlightr")
  if (template_path == "") {
    stop("Template not found: ", name)
  }
  template <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
  whisker::whisker.render(template, data)
}

#' Copy a static template from inst/templates/ to a destination
#'
#' @param name Template filename (e.g. "gitignore")
#' @param dest Destination file path
#' @keywords internal
copy_template <- function(name, dest) {
  template_path <- system.file("templates", name, package = "starlightr")
  if (template_path == "") {
    stop("Template not found: ", name)
  }
  if (!file.copy(template_path, dest, overwrite = TRUE)) {
    stop("Failed to copy template '", name, "' to: ", dest)
  }
}

#' Preview the generated site
#'
#' @param output_path Path to built site
#' @keywords internal
preview_site <- function(output_path) {
  cli::cli_h2("To preview your site")
  cli::cli_ol(c(
    "cd {.path {output_path}}",
    "npm install",
    "npm run dev"
  ))
}
