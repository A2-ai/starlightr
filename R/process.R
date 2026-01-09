# Content processing functions for starlightr

#' Process package documentation into Starlight format
#'
#' @param pkg_path Path to package directory
#' @param output_path Path to output directory
#' @param config Configuration list
#' @param verbose Logical, whether to print debug messages for example capture
#' @keywords internal
process_package_documentation <- function(pkg_path, output_path, config, verbose = FALSE) {
  cli::cli_alert_info("Processing R documentation...")

  # Get the actual package name from DESCRIPTION
  pkg_name <- get_package_name(pkg_path)

  # Get Rd database directly from tools (new approach)
  rd_db <- tryCatch(
    tools::Rd_db(pkg_name),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to load Rd documentation for package {.pkg {pkg_name}}",
        "i" = "Make sure the package is installed: {.code install.packages('{pkg_name}')}"
      ))
    }
  )

  if (length(rd_db) == 0) {
    cli::cli_warn("No Rd files found in package {.pkg {pkg_name}}")
    return()
  }

  # Capture example outputs before generating markdown
  cli::cli_alert_info("Capturing example outputs...")
  artifact_dir <- file.path(output_path, "public", "examples")
  text_output_dir <- file.path(output_path, "public", "examples", "text")

  tryCatch({
    capture_example_output(pkg_name, artifact_dir, text_output_dir, verbose = verbose)
    cli::cli_alert_success("Example outputs captured successfully")
  }, error = function(e) {
    cli::cli_warn(c(
      "Could not capture example outputs: {e$message}",
      "i" = "Examples will show code only (no outputs)"
    ))
  })

  # Generate markdown files for each function
  ref_dir <- file.path(output_path, "src", "content", "docs", "reference")

  # Build config for rd_to_markdown
  md_config <- list(
    skip_sections = config$content$skip_sections %||% c("alias", "keyword", "concept"),
    arguments_format = "table",
    include_title = TRUE,
    include_frontmatter = TRUE
  )

  write_md_files(
    rd_db = rd_db,
    output_dir = ref_dir,
    file_ext = ".mdx",
    config = md_config,
    site_output_path = output_path,
    pkg_name = pkg_name
  )

  cli::cli_alert_success("Generated reference documentation for {length(rd_db)} functions")
}

#' Process articles from vignettes/*.Rmd
#'
#' @param pkg_path Path to package directory
#' @param output_path Path to output directory
#' @param config Configuration list
#' @keywords internal
process_articles <- function(pkg_path, output_path, config) {
  articles_source <- file.path(pkg_path, "vignettes")

  if (!dir.exists(articles_source)) {
    cli::cli_alert_info("No vignettes/ directory found, skipping articles")
    return()
  }

  # Find all .Rmd files
  rmd_files <- list.files(articles_source, pattern = "\\.Rmd$", full.names = TRUE)

  if (length(rmd_files) == 0) {
    cli::cli_alert_info("No .Rmd files found in vignettes/")
    return()
  }

  cli::cli_alert_info("Processing articles...")
  articles_dir <- file.path(output_path, "src", "content", "docs", "articles")

  for (rmd_file in rmd_files) {
    process_rmd_file(rmd_file, articles_dir)
  }

  cli::cli_alert_success("Generated {length(rmd_files)} article{?s}")
}

#' Process a single .Rmd file to Markdown
#'
#' @param rmd_path Path to .Rmd file
#' @param output_dir Output directory for markdown file
#' @keywords internal
process_rmd_file <- function(rmd_path, output_dir) {
  # Properly render Rmd using rmarkdown/knitr
  base_name <- tools::file_path_sans_ext(basename(rmd_path))

  # Create temporary directory for rendering
  temp_dir <- tempdir()
  temp_md <- file.path(temp_dir, paste0(base_name, ".md"))

  # Render Rmd to Markdown using GFM variant (fenced code blocks for MDX compatibility)
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    rmarkdown::render(
      input = rmd_path,
      output_format = rmarkdown::md_document(variant = "gfm", preserve_yaml = FALSE),
      output_file = temp_md,
      quiet = TRUE
    )
  } else if (requireNamespace("knitr", quietly = TRUE)) {
    # Fallback to knitr if rmarkdown not available
    knitr::knit(input = rmd_path, output = temp_md, quiet = TRUE)
  } else {
    stop("Neither rmarkdown nor knitr is available. Install one of these packages to process vignettes.")
  }

  # Read the rendered markdown
  rendered_content <- readLines(temp_md, warn = FALSE)

  # Post-process markdown for MDX compatibility
  rendered_md <- paste(rendered_content, collapse = "\n")

  # Fix lifecycle badge image paths (use CDN)
  rendered_md <- gsub(
    "\\.\\./help/figures/lifecycle-([a-z]+)\\.svg",
    "https://lifecycle.r-lib.org/articles/figures/lifecycle-\\1.svg",
    rendered_md
  )
  rendered_md <- gsub("\\.\\./help/figures/", "/figures/", rendered_md)

  # Escape angle brackets for MDX compatibility
  rendered_md <- escape_angle_brackets(rendered_md)
  rendered_content <- strsplit(rendered_md, "\n", fixed = TRUE)[[1]]

  # Convert to MDX and clean up frontmatter
  output_file <- file.path(output_dir, paste0(base_name, ".mdx"))

  # Add simple frontmatter since we stripped the original
  title <- tools::toTitleCase(gsub("[-_]", " ", base_name))
  final_content <- c("---", paste0("title: \"", title, "\""), "---", "", rendered_content)

  writeLines(final_content, output_file)

  # Clean up temp file
  if (file.exists(temp_md)) {
    unlink(temp_md)
  }
}

#' Process NEWS.md file
#'
#' @param pkg_path Path to package directory
#' @param output_path Path to output directory
#' @param config Configuration list
#' @keywords internal
process_news <- function(pkg_path, output_path, config) {
  news_config <- config$sidebar$news

  # Get source file (default NEWS.md)
  source_file <- news_config$source %||% "NEWS.md"
  news_path <- file.path(pkg_path, source_file)

  if (!file.exists(news_path)) {
    cli::cli_warn("NEWS file not found: {.path {news_path}}")
    return()
  }

  cli::cli_alert_info("Processing {.file {source_file}}...")

  # Read the NEWS.md content
  news_content <- readLines(news_path, warn = FALSE)

  # Check if it already has frontmatter
  has_frontmatter <- length(news_content) > 0 && news_content[1] == "---"

  if (!has_frontmatter) {
    # Add frontmatter
    label <- news_config$label %||% "Changelog"
    news_content <- c(
      "---",
      paste0('title: "', label, '"'),
      "---",
      "",
      news_content
    )
  }

  # Write to docs directory
  docs_dir <- file.path(output_path, "src", "content", "docs")
  output_file <- file.path(docs_dir, "news.mdx")

  writeLines(news_content, output_file)
  cli::cli_alert_success("Generated {.file news.mdx}")
}
