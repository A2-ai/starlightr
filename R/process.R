# Content processing functions for starlightr

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
      "pagefind: true",
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
