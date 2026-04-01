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

  # Extract and process R documentation
  process_package_documentation(
    pkg_path,
    output_path,
    config_path,
    verbose = verbose
  )

  # Process vignettes and README together (single install)
  process_articles_and_readme(pkg_path, output_path, config)

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
