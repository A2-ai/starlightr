# Utility functions for starlightr

# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Convert a name to a URL-safe slug
#'
#' Lowercases the input and replaces dots with hyphens.
#' Used for generating consistent reference and article slugs.
#'
#' @param name Character string to slugify
#' @return Slugified character string
#' @keywords internal
#' @noRd
slugify <- function(name) {
  gsub(".", "-", tolower(name), fixed = TRUE)
}

#' Derive the Starlight site root from a content output directory
#'
#' Strips a trailing `src/content/docs/...` segment so that, e.g.,
#' `../my-site/src/content/docs/articles` resolves to `../my-site`. Returns
#' `output_dir` unchanged if the segment is not present.
#'
#' @param output_dir Path to a content directory inside the site
#' @return Path to the site root
#' @keywords internal
#' @noRd
resolve_site_dir <- function(output_dir) {
  site_dir <- sub("[/\\\\]src[/\\\\]content[/\\\\]docs([/\\\\].*)?$", "", output_dir)
  if (identical(site_dir, output_dir)) {
    cli::cli_warn(c(
      "Could not derive site root from {.path {output_dir}}.",
      i = "Figures will be written under {.path {file.path(output_dir, 'public', 'figures')}}; pass {.arg site_dir} to control this."
    ))
  }
  site_dir
}

#' Create a directory, aborting on failure
#'
#' @param path Directory path to create
#' @return Invisibly returns the path
#' @keywords internal
#' @noRd
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(path)) {
      cli::cli_abort("Failed to create directory: {.path {path}}")
    }
  }
  invisible(path)
}

#' Check if a path is absolute
#'
#' @param path Character string representing a file path
#' @return Logical indicating if the path is absolute
#' @keywords internal
#' @noRd
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
#' @noRd
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

#' Find README (.Rmd or .md) in package root or inst/
#'
#' @param pkg_path Path to package directory
#' @return README path if found, otherwise NULL
#' @keywords internal
#' @noRd
find_readme <- function(pkg_path) {
  candidates <- c(
    file.path(pkg_path, "README.Rmd"),
    file.path(pkg_path, "inst", "README.Rmd"),
    file.path(pkg_path, "README.md"),
    file.path(pkg_path, "inst", "README.md")
  )

  for (path in candidates) {
    if (file.exists(path)) {
      return(path)
    }
  }

  NULL
}

#' Escape a string for use inside a quoted string ("...")
#'
#' Handles backslashes, double quotes, and control characters.
#' Works for TOML basic strings, YAML double-quoted strings, and JS string literals.
#'
#' @param x Character string to escape
#' @return Escaped string safe for use inside double quotes
#' @keywords internal
#' @noRd
escape_quoted_string <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  x
}

#' Extract GitHub URL from configuration
#'
#' @param config Configuration list
#' @return GitHub URL or NULL
#' @keywords internal
#' @noRd
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

#' Get a specific section from an Rd object
#'
#' @param rd_obj Rd object from tools::Rd_db
#' @param tag Section tag (e.g., "description", "arguments")
#'
#' @return The Rd element for that section, or NULL if not found
#' @keywords internal
#' @noRd
get_rd_section <- function(rd_obj, tag) {
  if (!startsWith(tag, "\\")) {
    tag <- paste0("\\", tag)
  }

  for (element in rd_obj) {
    element_tag <- attr(element, "Rd_tag")
    if (!is.null(element_tag) && element_tag == tag) {
      return(element)
    }
  }
  NULL
}

#' Fix lifecycle badge paths to use CDN
#'
#' Rewrites lifecycle badge image paths from various local/relative formats
#' to the canonical r-lib CDN URL.
#'
#' @param md Markdown string
#' @return Markdown with lifecycle badges pointing to CDN
#' @keywords internal
#' @noRd
fix_lifecycle_badges <- function(md) {
  cdn_base <- "https://lifecycle.r-lib.org/articles/figures/lifecycle-"

  md <- gsub(
    "man/figures/lifecycle-([a-z]+)\\.svg",
    paste0(cdn_base, "\\1.svg"),
    md
  )

  md <- gsub(
    "\\.\\.(/[^)\\s]+)?/lifecycle-([a-z]+)\\.svg",
    paste0(cdn_base, "\\2.svg"),
    md,
    perl = TRUE
  )

  md
}

#' Swap img height attributes to width
#'
#' Starlight's markdown CSS sets `height: auto` on content images, which
#' overrides an HTML `height` attribute and leaves the image at full size.
#' READMEs written for GitHub commonly size logos that way, so rename the
#' attribute. Tags that already set `width` are left alone.
#'
#' @param md Markdown string
#' @return Markdown with `height`-only img tags sized by `width` instead
#' @keywords internal
#' @noRd
fix_img_width <- function(md) {
  gsub(
    "(?i)<img(?![^>]*\\swidth\\s*=)([^>]*?)\\sheight\\s*=",
    "<img\\1 width=",
    md,
    perl = TRUE
  )
}

#' Rewrite cross-vignette .html links to article page URLs
#'
#' Vignettes link to each other with `[Title](other.html)`, which is how
#' `R CMD build` lays them out side by side in `inst/doc/`. Each article is
#' published as its own directory, so the link has to climb one level and
#' point at the sibling page. Only targets that were actually built are
#' rewritten; anything else is left alone rather than turned into a link
#' that looks valid and 404s.
#'
#' @param md Markdown string
#' @param link_targets Named character vector mapping a built article's
#'   source name (the `.html` stem a vignette would link to) to its slug
#' @return Markdown with cross-article links pointing at `../<slug>/`
#' @keywords internal
#' @noRd
rewrite_article_links <- function(md, link_targets) {
  for (name in names(link_targets)) {
    pattern <- paste0(
      "\\]\\(",
      escape_regex(name),
      "\\.html(#[^)]*)?\\)"
    )
    md <- gsub(
      pattern,
      paste0("](../", link_targets[[name]], "/\\1)"),
      md,
      perl = TRUE
    )
  }
  md
}

#' Rewrite cross-vignette links across every built article
#'
#' @param files Character vector of written article paths
#' @param link_targets Named character vector of source name to slug
#' @return Invisibly, `files`
#' @keywords internal
#' @noRd
fix_article_links <- function(files, link_targets) {
  if (length(files) == 0 || length(link_targets) == 0) {
    return(invisible(files))
  }

  for (f in files) {
    md <- paste(readLines(f, warn = FALSE), collapse = "\n")
    updated <- rewrite_article_links(md, link_targets)
    if (!identical(updated, md)) {
      writeLines(updated, f)
    }
  }

  invisible(files)
}

#' Escape regex metacharacters in a literal string
#'
#' @param x Character string
#' @return `x` with metacharacters backslash-escaped
#' @keywords internal
#' @noRd
escape_regex <- function(x) {
  gsub("([.\\\\|()\\[\\]{}^$*+?-])", "\\\\\\1", x, perl = TRUE)
}

#' Does an Rd file carry \\keyword{internal}?
#'
#' @param path Path to an .Rd file
#' @return `TRUE` when the topic is marked internal
#' @keywords internal
#' @noRd
is_internal_rd <- function(path) {
  content <- readLines(path, warn = FALSE)
  any(grepl("\\\\keyword\\{internal\\}", content))
}

#' Does an Rd file document a dataset?
#'
#' @param path Path to an .Rd file
#' @return `TRUE` when the topic is a dataset (`\\docType{data}`)
#' @keywords internal
#' @noRd
is_dataset_rd <- function(path) {
  content <- readLines(path, warn = FALSE)
  any(grepl("\\\\docType\\{data\\}", content))
}

#' Render a whisker template from inst/templates/
#'
#' @param name Template filename (e.g. "astro.config.mjs")
#' @param data Named list of variables for whisker interpolation
#' @return Rendered template as a character string
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
preview_site <- function(output_path) {
  cli::cli_h2("To preview your site")
  cli::cli_ol(c(
    "cd {.path {output_path}}",
    "bun install",
    "bun run dev"
  ))
}
