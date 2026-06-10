#' Build a Starlight documentation site for an R package
#'
#' @param pkg Path to package directory, defaults to current directory
#' @param config_file Path to _starlightr.toml configuration file
#' @param output_dir Directory to build the site in
#' @param preview Logical, whether to open preview after building
#' @param verbose Logical, whether to print debug messages for example capture
#' @param overwrite Logical, whether to overwrite existing files like index.mdx
#'
#' @return Invisibly returns the path to the built site
#' @export
#'
#' @examples \dontrun{
#' # Build site for current package (uses config default)
#' build_site()
#'
#' # Build to external directory (recommended to avoid bloat)
#' build_site(output_dir = "/path/to/my-package-docs")
#' build_site(output_dir = "../my-package-docs")
#'
#' # Build with custom configuration
#' build_site(config_file = "my_config.toml")
#'
#' # Build to relative directory (not recommended due to Starlight bloat)
#' build_site(output_dir = "docs")
#'
#' # Build with verbose example output
#' build_site(verbose = TRUE)
#'
#' # Rebuild and overwrite existing index.mdx
#' build_site(overwrite = TRUE)
#' }
build_site <- function(
  pkg = ".",
  config_file = "_starlightr.toml",
  output_dir = NULL,
  preview = FALSE,
  verbose = FALSE,
  overwrite = FALSE
) {
  # Resolve package path
  pkg_path <- normalizePath(pkg, mustWork = TRUE)

  # Require config file for builds
  config_path <- file.path(pkg_path, config_file)
  if (!file.exists(config_path)) {
    cli::cli_abort("Configuration file not found at {.path {config_path}}")
  }

  # Audit configuration early so users see issues before generation starts
  audit_config(pkg_path, config_file)

  # Read configuration
  config <- read_config(config_path)

  # Determine output directory
  if (is.null(output_dir)) {
    output_dir <- config$output$dir %||% "docs"
  }

  # Handle absolute vs relative paths
  if (is_absolute_path(output_dir)) {
    output_path <- output_dir
  } else {
    output_path <- file.path(pkg_path, output_dir)
  }

  cli::cli_h1("Building Starlight site for {.pkg {basename(pkg_path)}}")
  cli::cli_alert_info("Output directory: {.path {output_path}}")

  # Provide helpful info about external vs internal directories
  if (is_absolute_path(output_dir) || startsWith(output_dir, "..")) {
    cli::cli_alert_info(
      "Using external directory (recommended for Starlight sites)"
    )
  } else {
    cli::cli_alert_warning(
      "Using internal directory - consider external directory to avoid bloat"
    )
  }

  # Create output directory structure
  setup_starlight_structure(output_path, config)

  # Generate initial configuration files (without astro.config.mjs since we need files first)
  generate_content_config(output_path)
  generate_starlightr_css(output_path)
  generate_custom_css(output_path)
  if (config$output$include_build_files %||% TRUE) {
    generate_remark_plugin(output_path)
    generate_package_json(output_path, config, overwrite = overwrite)
    generate_gitignore(output_path, overwrite = overwrite)
  }

  # Add version support files if configured
  if (has_version_support(config)) {
    validate_version_config(config)
    generate_versions_ts(output_path, config)
    generate_version_select_component(output_path, config)

    # Generate GitHub workflow if requested
    # Generate workflow by default unless explicitly disabled
    if (!isFALSE(config$versions$workflow)) {
      generate_deploy_workflow(output_path, config, overwrite = overwrite)
    }
  }

  # Build reference documentation (config-filtered, with inline examples)
  ref_output <- file.path(output_path, "src", "content", "docs", "reference")
  rd_files <- resolve_config_rd_files(pkg_path, config)
  if (length(rd_files) > 0) {
    build_reference_files(
      rd_files = rd_files,
      output_dir = ref_output,
      pkg = pkg_path,
      config_file = config_file,
      examples = TRUE,
      verbose = verbose
    )
  }

  # Build articles (config-filtered, figures copied to public/figures/)
  articles_output <- file.path(output_path, "src", "content", "docs", "articles")
  rmd_files <- resolve_config_rmd_files(pkg_path, config)
  if (length(rmd_files) > 0) {
    build_articles(
      rmd_files = rmd_files,
      output_dir = articles_output,
      site_dir = output_path,
      verbose = verbose
    )
  }

  # Process NEWS.md if configured
  if (!is.null(config$sidebar$news)) {
    process_news(pkg_path, output_path, config)
  }

  # Generate astro.config.mjs now that files exist for pattern matching
  generate_astro_config(output_path, config, pkg_path)

  # Copy assets if they exist
  copy_assets(pkg_path, output_path, config)

  # Copy logo and favicon if configured
  copy_branding_assets(pkg_path, output_path, config)

  # Create default index.mdx if it doesn't exist (or overwrite if requested)
  create_index_page(pkg_path, output_path, config, overwrite = overwrite)

  cli::cli_alert_success("Site built successfully!")

  if (preview) {
    preview_site(output_path)
  }

  invisible(output_path)
}

#' Resolve config sidebar.reference to .Rd file paths
#'
#' If config has sidebar.reference, resolves slugs/patterns to .Rd files.
#' Otherwise returns all .Rd files (minus internal unless configured).
#'
#' @param pkg_path Package directory path
#' @param config Parsed config list
#' @return Character vector of .Rd file paths
#' @keywords internal
resolve_config_rd_files <- function(pkg_path, config) {
  rd_dir <- file.path(pkg_path, "man")
  if (!dir.exists(rd_dir)) return(character())

  all_rd <- list.files(rd_dir, pattern = "\\.Rd$", full.names = TRUE)
  if (length(all_rd) == 0) return(character())

  # Filter internal unless config says otherwise
  include_internal <- config$reference$include_internal %||% FALSE
  if (!include_internal) {
    all_rd <- Filter(function(f) {
      content <- readLines(f, warn = FALSE)
      !any(grepl("\\\\keyword\\{internal\\}", content))
    }, all_rd)
  }

  # If no sidebar.reference config, return all
  config_refs <- extract_config_references(config)
  if (length(config_refs) == 0) return(all_rd)

  # Resolve config refs (slugs/patterns) against available .Rd basenames
  rd_basenames <- tools::file_path_sans_ext(basename(all_rd))
  matched <- character()
  for (ref in config_refs) {
    if (grepl("\\*", ref)) {
      pattern <- paste0("^", gsub("\\*", ".*", ref), "$")
      hits <- rd_basenames[grepl(pattern, rd_basenames, ignore.case = TRUE)]
    } else {
      hits <- rd_basenames[tolower(rd_basenames) == tolower(ref)]
    }
    matched <- c(matched, hits)
  }

  matched <- unique(matched)
  all_rd[rd_basenames %in% matched]
}

#' Resolve config sidebar.articles to .Rmd file paths
#'
#' If config has sidebar.articles, resolves slugs to .Rmd files.
#' Otherwise discovers all vignettes + README.
#'
#' @param pkg_path Package directory path
#' @param config Parsed config list
#' @return Character vector of .Rmd file paths
#' @keywords internal
resolve_config_rmd_files <- function(pkg_path, config) {
  # Extract article slugs from config
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

  # If no config, discover all vignettes + README
  if (length(article_names) == 0) {
    rmd_files <- character()
    vignettes_dir <- file.path(pkg_path, "vignettes")
    if (dir.exists(vignettes_dir)) {
      rmd_files <- list.files(vignettes_dir, pattern = "\\.Rmd$", full.names = TRUE)
    }
    readme_path <- find_readme(pkg_path)
    if (!is.null(readme_path)) {
      rmd_files <- c(readme_path, rmd_files)
    }
    return(rmd_files)
  }

  # Resolve slugs to file paths
  vignettes_dir <- file.path(pkg_path, "vignettes")
  rmd_files <- character()
  for (name in article_names) {
    if (tolower(name) == "readme") {
      path <- find_readme(pkg_path)
      if (!is.null(path)) rmd_files <- c(rmd_files, path)
    } else {
      path <- file.path(vignettes_dir, paste0(name, ".Rmd"))
      if (file.exists(path)) rmd_files <- c(rmd_files, path)
    }
  }

  rmd_files
}
