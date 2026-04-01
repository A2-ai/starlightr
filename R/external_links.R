# Build external link map for Rust-side resolution

# Base R packages use rdrr.io/r/ instead of rdrr.io/pkg/
.base_packages <- c(
  "base", "compiler", "datasets", "graphics", "grDevices", "grid",
  "methods", "parallel", "splines", "stats", "stats4", "tcltk",
  "tools", "utils"
)

#' Build an external link map from installed package aliases
#'
#' Reads DESCRIPTION to find Imports and Suggests, then builds a JSON file
#' mapping "pkg::topic" to documentation URLs for all aliases in those packages.
#'
#' @param package Path to the package root (containing DESCRIPTION)
#' @return Path to a temporary JSON file containing the link map
#' @keywords internal
build_external_link_map <- function(package = ".") {
  desc <- read.dcf(file.path(package, "DESCRIPTION"))
  deps <- character()
  for (field in c("Imports", "Suggests", "Depends")) {
    if (field %in% colnames(desc) && !is.na(desc[, field])) {
      deps <- c(deps, trimws(strsplit(desc[, field], ",")[[1]]))
    }
  }
  # Strip version constraints
  deps <- sub("\\s*\\(.*\\)", "", deps)
  deps <- deps[deps != "" & deps != "R"]
  deps <- unique(deps)

  link_map <- list()
  for (pkg in deps) {
    aliases_path <- system.file("help", "aliases.rds", package = pkg)
    if (aliases_path == "") next

    aliases <- readRDS(aliases_path)
    rdnames <- unname(aliases)
    topics <- names(aliases)

    base_url <- resolve_package_base_url(pkg)

    for (i in seq_along(topics)) {
      key <- paste0(pkg, "::", topics[i])
      link_map[[key]] <- paste0(base_url, "/", rdnames[i], ".html")
    }
  }

  json_path <- tempfile("starlightr-links-", fileext = ".json")
  writeLines(jsonlite::toJSON(link_map, auto_unbox = TRUE), json_path)
  json_path
}

#' Resolve the base reference URL for a package
#'
#' Reads the URL field from an installed package's DESCRIPTION and constructs
#' the reference documentation base URL. Falls back to rdrr.io.
#'
#' @param pkg Package name
#' @return Base URL string for reference docs (no trailing slash)
#' @keywords internal
resolve_package_base_url <- function(pkg) {
  if (pkg %in% .base_packages) {
    return(paste0("https://rdrr.io/r/", pkg))
  }

  desc_path <- system.file("DESCRIPTION", package = pkg)
  if (desc_path == "") {
    return(paste0("https://rdrr.io/pkg/", pkg, "/man"))
  }

  url_field <- read.dcf(desc_path, fields = "URL")[[1]]
  if (is.na(url_field)) {
    return(paste0("https://rdrr.io/pkg/", pkg, "/man"))
  }

  urls <- trimws(strsplit(url_field, "[,\\s]+", perl = TRUE)[[1]])
  urls <- urls[grepl("^https?://", urls)]
  # Filter out common non-doc URLs (GitHub, GitLab, bug trackers)
  doc_urls <- urls[!grepl("github\\.com|gitlab\\.com|bugs\\.", urls)]

  if (length(doc_urls) > 0) {
    paste0(doc_urls[1], "/reference")
  } else if (length(urls) > 0) {
    paste0("https://rdrr.io/pkg/", pkg, "/man")
  } else {
    paste0("https://rdrr.io/pkg/", pkg, "/man")
  }
}
