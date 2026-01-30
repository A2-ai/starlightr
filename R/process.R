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

#' Process articles (vignettes) and README together
#'
#' Builds all Rmd files in a single devtools::build_rmd() call to avoid
#' multiple package installs. Processes vignettes from config and README.Rmd.
#' "readme" in the config is treated specially - maps to README.Rmd at package root.
#'
#' @param pkg_path Path to package directory
#' @param output_path Path to output directory
#' @param config Configuration list
#' @keywords internal
process_articles_and_readme <- function(pkg_path, output_path, config) {
  vignettes_dir <- file.path(pkg_path, "vignettes")
  articles_dir <- file.path(output_path, "src", "content", "docs", "articles")
  dir.create(articles_dir, recursive = TRUE, showWarnings = FALSE)

  # Collect article names from config (extract slugs from content items)
  article_names <- character(0)
  if (!is.null(config$sidebar$articles)) {
    articles_config <- config$sidebar$articles
    for (group in articles_config) {
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
    return()
  }

  # Map article names to Rmd file paths
  # "readme" is special - maps to README.Rmd at package root
  rmd_paths <- character(length(article_names))
  for (i in seq_along(article_names)) {
    name <- article_names[i]
    if (tolower(name) == "readme") {
      # Check package root, then inst/
      readme_path <- file.path(pkg_path, "README.Rmd")
      if (!file.exists(readme_path)) {
        readme_path <- file.path(pkg_path, "inst", "README.Rmd")
      }
      rmd_paths[i] <- readme_path
    } else {
      rmd_paths[i] <- file.path(vignettes_dir, paste0(name, ".Rmd"))
    }
  }

  # Check which files exist
  existing <- file.exists(rmd_paths)
  if (any(!existing)) {
    for (f in rmd_paths[!existing]) {
      cli::cli_warn("Article Rmd not found: {.file {f}}")
    }
  }

  rmd_paths <- rmd_paths[existing]
  article_names <- article_names[existing]

  if (length(rmd_paths) == 0) {
    cli::cli_alert_info("No article Rmd files found")
    return()
  }

  # Collect all Rmd files to build
  all_rmds <- rmd_paths

  # Build all Rmds in one call (single install) into a temp directory
  # Explicitly use md_document to preserve raw LaTeX (not render to HTML)
  build_dir <- tempfile("starlightr-rmd-")
  dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)

  cli::cli_alert_info("Building {length(all_rmds)} Rmd file{?s}...")
  devtools::build_rmd(
    all_rmds,
    output_format = rmarkdown::md_document(
      variant = "gfm",
      preserve_yaml = FALSE
    ),
    output_dir = build_dir,
    quiet = TRUE
  )

  # Copy man/figures/ from package to output (common for README badges/images)
  man_figures <- file.path(pkg_path, "man", "figures")
  if (dir.exists(man_figures)) {
    dest_figures <- file.path(output_path, "public", "figures")
    dir.create(dest_figures, recursive = TRUE, showWarnings = FALSE)
    file.copy(
      list.files(man_figures, full.names = TRUE),
      dest_figures,
      overwrite = TRUE
    )
  }

  # Process each article output
  for (i in seq_along(article_names)) {
    name <- article_names[i]
    # Build output is in the temp directory
    source_dir <- build_dir
    md_name <- if (tolower(name) == "readme") "README" else name

    process_article_output(name, md_name, source_dir, output_path, articles_dir, config)
  }

  cli::cli_alert_success("Generated {length(article_names)} article{?s}")
}

#' Process a single article's built output (vignette or README)
#'
#' @param output_name Name for the output file (e.g., "readme", "introduction")
#' @param md_name Name of the .md file without extension (e.g., "README", "introduction")
#' @param source_dir Directory containing the .md file and _files folder
#' @param output_path Root output path for the site
#' @param articles_dir Output directory for article markdown files
#' @param config Configuration list
#' @keywords internal
process_article_output <- function(output_name, md_name, source_dir, output_path, articles_dir, config) {
  md_file <- file.path(source_dir, paste0(md_name, ".md"))
  if (!file.exists(md_file)) {
    cli::cli_warn("Built markdown not found: {.file {md_file}}")
    return()
  }

  md_content <- paste(readLines(md_file, warn = FALSE), collapse = "\n")

  # Copy figures from {md_name}_files/
  figures_dir <- file.path(source_dir, paste0(md_name, "_files"))
  dest_figures <- file.path(output_path, "public", "figures", tolower(output_name))
  dir.create(dest_figures, recursive = TRUE, showWarnings = FALSE)

  if (dir.exists(figures_dir)) {
    figure_gfm <- file.path(figures_dir, "figure-gfm")
    if (dir.exists(figure_gfm)) {
      file.copy(list.files(figure_gfm, full.names = TRUE), dest_figures, overwrite = TRUE)
    }
  }

  # Also check for custom fig.path figures (e.g., figures/{name}/)
  custom_figures_dir <- file.path(source_dir, "figures")
  if (dir.exists(custom_figures_dir)) {
    fig_subdirs <- list.dirs(custom_figures_dir, recursive = FALSE, full.names = TRUE)
    for (subdir in fig_subdirs) {
      subdir_name <- basename(subdir)
      subdir_dest <- file.path(output_path, "public", "figures", subdir_name)
      dir.create(subdir_dest, recursive = TRUE, showWarnings = FALSE)
      file.copy(list.files(subdir, full.names = TRUE), subdir_dest, overwrite = TRUE)
    }
    # Also copy any files directly in figures/
    fig_files <- list.files(custom_figures_dir, full.names = TRUE, recursive = FALSE)
    fig_files <- fig_files[!dir.exists(fig_files)]
    if (length(fig_files) > 0) {
      file.copy(fig_files, dest_figures, overwrite = TRUE)
    }
  }

  # Rewrite figure paths - use absolute paths (Astro base config handles versioning)
  md_content <- gsub(
    paste0(md_name, "_files/figure-gfm/"),
    paste0("/figures/", tolower(output_name), "/"),
    md_content, fixed = TRUE
  )

  # Rewrite temp directory figure paths (cross-platform: matches starlightr-rmd- marker)
  # Capture the markdown image prefix ![...]( and restore it
  md_content <- gsub(
    "(!\\[[^]]*\\]\\()[^)]*starlightr-rmd-[^/\\\\]+[/\\\\]+figures[/\\\\]",
    "\\1/figures/",
    md_content, perl = TRUE
  )

  # Fix lifecycle badges (must come BEFORE generic man/figures/ rewrite)
  md_content <- fix_lifecycle_badges(md_content)

  # Also handle man/figures/ paths (common in READMEs)
  md_content <- gsub("man/figures/", "/figures/", md_content, fixed = TRUE)

  # Remove HTML comments (can cause issues in some markdown parsers)
  md_content <- gsub("(?s)<!--.*?-->", "", md_content, perl = TRUE)

  # Add frontmatter - use config title for readme, otherwise generate from name
  if (tolower(output_name) == "readme") {
    title <- config$readme$title %||% "Getting Started"
  } else {
    title <- tools::toTitleCase(gsub("[-_]", " ", output_name))
  }
  final_content <- paste0("---\ntitle: \"", title, "\"\npagefind: true\n---\n\n", md_content)

  # Use .md (not .mdx) so complex HTML like gt tables with KaTeX passes through
  # MDX's strict JSX parsing breaks on this content
  # Image paths are rewritten by remark plugin to include BASE_URL
  writeLines(final_content, file.path(articles_dir, paste0(tolower(output_name), ".md")))

  # Cleanup happens at the build directory level.
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
