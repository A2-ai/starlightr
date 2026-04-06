#' Build article markdown files from vignettes
#'
#' Renders `.Rmd` vignettes to markdown and post-processes them for use in a
#' Starlight site. Figures are embedded inline as base64 data URIs so only
#' `output_dir` is needed.
#'
#' @param articles Character vector of article names. The special value
#'   `"readme"` maps to `README.Rmd` at the package root; all other names map
#'   to `vignettes/{name}.Rmd`.
#' @param output_dir Path to directory where article `.md` files are saved.
#' @param pkg Path to the package directory (default `"."`).
#' @param config_file Path to `_starlightr.toml` (relative to `pkg`).
#' @param verbose Logical, whether to print debug messages during Rmd
#'   rendering (default `FALSE`).
#'
#' @return Invisibly returns a character vector of written file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' build_articles(
#'   c("readme", "introduction"),
#'   output_dir = "../my-site/src/content/docs/articles"
#' )
#' }
build_articles <- function(
  articles,
  output_dir,
  pkg = ".",
  config_file = "_starlightr.toml",
  verbose = FALSE
) {
  pkg_path <- normalizePath(pkg, mustWork = TRUE)
  config_path <- file.path(pkg_path, config_file)
  config <- if (file.exists(config_path)) read_config(config_path) else list()
  vignettes_dir <- file.path(pkg_path, "vignettes")

  ensure_dir(output_dir)

  # Map article names to Rmd file paths
  rmd_paths <- character(length(articles))
  for (i in seq_along(articles)) {
    name <- articles[i]
    if (tolower(name) == "readme") {
      readme_path <- find_readme(pkg_path)
      rmd_paths[i] <- readme_path %||% file.path(pkg_path, "README.Rmd")
    } else {
      rmd_paths[i] <- file.path(vignettes_dir, paste0(name, ".Rmd"))
    }
  }

  # Validate existence
  existing <- file.exists(rmd_paths)
  if (any(!existing)) {
    for (f in rmd_paths[!existing]) {
      cli::cli_warn("Article Rmd not found: {.file {f}}")
    }
  }
  rmd_paths <- rmd_paths[existing]
  articles <- articles[existing]

  if (length(rmd_paths) == 0) {
    cli::cli_alert_info("No article Rmd files found")
    return(invisible(character()))
  }

  # Split into Rmd (need building) and pre-rendered Md (just copy)
  is_rmd <- grepl("\\.Rmd$", rmd_paths, ignore.case = TRUE)
  rmd_to_build <- rmd_paths[is_rmd]
  md_to_copy <- rmd_paths[!is_rmd]

  # Build all Rmds in one call (single install) into a temp directory
  build_dir <- tempfile("starlightr-rmd-")
  ensure_dir(build_dir)
  on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)

  if (length(rmd_to_build) > 0) {
    cli::cli_alert_info("Building {length(rmd_to_build)} Rmd file{?s}...")
    devtools::build_rmd(
      rmd_to_build,
      output_format = rmarkdown::md_document(
        variant = "gfm",
        preserve_yaml = FALSE
      ),
      output_dir = build_dir,
      quiet = !verbose
    )
  }

  # Copy pre-rendered Markdown files directly to build dir
  if (length(md_to_copy) > 0) {
    cli::cli_alert_info(
      "Copying {length(md_to_copy)} pre-rendered Markdown file{?s}..."
    )
    file.copy(md_to_copy, build_dir, overwrite = TRUE)
  }

  # Process each article
  written_files <- character()
  for (i in seq_along(articles)) {
    name <- articles[i]
    md_name <- if (tolower(name) == "readme") "README" else name

    out_file <- process_article_inline(
      name,
      md_name,
      build_dir,
      output_dir,
      config
    )
    if (!is.null(out_file)) {
      written_files <- c(written_files, out_file)
    }
  }

  cli::cli_alert_success("Generated {length(written_files)} article{?s}")
  invisible(written_files)
}

#' Build all configured articles for a package
#'
#' Convenience wrapper around [build_articles()] that reads article names from
#' the `_starlightr.toml` configuration.
#'
#' @inheritParams build_articles
#'
#' @return Invisibly returns a character vector of written file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' build_package_articles(
#'   output_dir = "../my-site/src/content/docs/articles"
#' )
#' }
build_package_articles <- function(
  output_dir,
  pkg = ".",
  config_file = "_starlightr.toml",
  verbose = FALSE
) {
  pkg_path <- normalizePath(pkg, mustWork = TRUE)
  config_path <- file.path(pkg_path, config_file)

  if (!file.exists(config_path)) {
    cli::cli_abort("Configuration file not found at {.path {config_path}}")
  }

  config <- read_config(config_path)

  # Extract article names from config
  article_names <- character(0)
  if (!is.null(config$sidebar$articles)) {
    for (group in config$sidebar$articles) {
      if (!is.null(group$contents)) {
        for (item in group$contents) {
          parsed <- parse_content_item(item)
          article_names <- c(article_names, parsed$slug)
        }
      }
    }
    article_names <- article_names[nchar(article_names) > 0]
  }

  if (length(article_names) == 0) {
    cli::cli_alert_info("No articles configured in sidebar.articles")
    return(invisible(character()))
  }

  build_articles(
    articles = article_names,
    output_dir = output_dir,
    pkg = pkg,
    config_file = config_file,
    verbose = verbose
  )
}

#' Process a single article's built output with inline figures
#'
#' Post-processes a built markdown file: inlines figures as base64 data URIs,
#' fixes paths, and adds YAML frontmatter.
#'
#' @param output_name Name for the output file (e.g., "readme", "introduction")
#' @param md_name Name of the .md file without extension
#' @param source_dir Directory containing the built .md and figure files
#' @param output_dir Directory where final .md is written
#' @param config Configuration list
#' @return Path to written file, or NULL if source not found
#' @keywords internal
process_article_inline <- function(
  output_name,
  md_name,
  source_dir,
  output_dir,
  config
) {
  md_file <- file.path(source_dir, paste0(md_name, ".md"))
  if (!file.exists(md_file)) {
    cli::cli_warn("Built markdown not found: {.file {md_file}}")
    return(NULL)
  }

  md_content <- paste(readLines(md_file, warn = FALSE), collapse = "\n")

  # Collect all figure files from known locations
  figure_files <- collect_article_figures(md_name, source_dir)

  # Inline all figure references as base64 data URIs
  md_content <- inline_figure_references(md_content, figure_files)

  # Fix lifecycle badges (must come BEFORE generic man/figures/ rewrite)
  md_content <- fix_lifecycle_badges(md_content)

  # Remove HTML comments
  md_content <- gsub("(?s)<!--.*?-->", "", md_content, perl = TRUE)

  # Add frontmatter
  if (tolower(output_name) == "readme") {
    title <- config$readme$title %||% "Getting Started"
  } else {
    title <- tools::toTitleCase(gsub("[-_]", " ", output_name))
  }
  final_content <- paste0(
    "---\ntitle: \"",
    title,
    "\"\npagefind: true\n---\n\n",
    md_content
  )

  out_file <- file.path(output_dir, paste0(tolower(output_name), ".md"))
  writeLines(final_content, out_file)
  out_file
}

#' Collect figure files from article build output
#'
#' Gathers all image files from standard knitr output locations.
#'
#' @param md_name Markdown filename without extension
#' @param source_dir Build output directory
#' @return Named list mapping relative paths (as they appear in markdown) to
#'   absolute file paths
#' @keywords internal
collect_article_figures <- function(md_name, source_dir) {
  figures <- list()

  # Standard knitr figure-gfm output
  figure_gfm <- file.path(source_dir, paste0(md_name, "_files"), "figure-gfm")
  if (dir.exists(figure_gfm)) {
    files <- list.files(figure_gfm, full.names = TRUE)
    for (f in files) {
      rel_path <- paste0(md_name, "_files/figure-gfm/", basename(f))
      figures[[rel_path]] <- f
    }
  }

  # Custom fig.path figures
  custom_figures <- file.path(source_dir, "figures")
  if (dir.exists(custom_figures)) {
    files <- list.files(custom_figures, recursive = TRUE, full.names = TRUE)
    files <- files[!dir.exists(files)]
    for (f in files) {
      rel_path <- file.path("figures", sub(
        paste0("^", normalizePath(custom_figures, mustWork = FALSE), .Platform$file.sep),
        "",
        normalizePath(f, mustWork = FALSE)
      ))
      figures[[rel_path]] <- f
    }
  }

  # man/figures (common in READMEs)
  # These are handled by path rewriting, not inlining

  figures
}

#' Inline figure references in markdown as base64 data URIs
#'
#' Finds markdown image references (`![alt](path)`) and replaces local file
#' paths with base64-encoded data URIs.
#'
#' @param md_content Markdown string
#' @param figure_files Named list from `collect_article_figures()`
#' @return Markdown with inline base64 images
#' @keywords internal
inline_figure_references <- function(md_content, figure_files) {
  for (rel_path in names(figure_files)) {
    abs_path <- figure_files[[rel_path]]
    if (!file.exists(abs_path)) next

    ext <- tolower(tools::file_ext(abs_path))
    mime <- switch(ext,
      png = "image/png",
      jpg = , jpeg = "image/jpeg",
      svg = "image/svg+xml",
      gif = "image/gif",
      "application/octet-stream"
    )

    raw_data <- readBin(abs_path, "raw", file.info(abs_path)$size)
    b64 <- base64enc::base64encode(raw_data)
    data_uri <- paste0("data:", mime, ";base64,", b64)

    # Replace the path in markdown (handle both exact and URL-encoded variants)
    md_content <- gsub(rel_path, data_uri, md_content, fixed = TRUE)
  }

  # Also handle temp directory figure paths that include the full temp path
  # These match patterns like /tmp/starlightr-rmd-XXXX/figures/...
  md_content <- gsub(
    "(!\\[[^]]*\\]\\()[^)]*starlightr-rmd-[^)]+\\)",
    "\\1)",
    md_content,
    perl = TRUE
  )

  md_content
}
