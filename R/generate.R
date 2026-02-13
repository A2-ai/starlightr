# File generation functions for starlightr

#' Set up Starlight directory structure
#'
#' @param output_path Path to output directory
#' @param config Configuration list
#' @keywords internal
setup_starlight_structure <- function(output_path, config) {
  # Create main directories
  dirs <- c(
    output_path,
    file.path(output_path, "src"),
    file.path(output_path, "src", "content"),
    file.path(output_path, "src", "content", "docs"),
    file.path(output_path, "src", "content", "docs", "reference"),
    file.path(output_path, "public")
  )

  # Only create articles dir if configured
  if (!is.null(config$sidebar$articles)) {
    dirs <- c(dirs, file.path(output_path, "src", "content", "docs", "articles"))
  }

  # Add version support directories if enabled
  if (has_version_support(config)) {
    dirs <- c(dirs,
      file.path(output_path, "src", "components"),
      file.path(output_path, "src", "data")
    )
  }

  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
}

#' Generate astro.config.mjs file
#'
#' @param output_path Path to output directory
#' @param config Configuration list
#' @param pkg_path Path to package directory
#' @keywords internal
generate_astro_config <- function(output_path, config, pkg_path = NULL) {
  github_url <- get_github_url(config)

  # Generate sidebar configuration from YAML
  pkg_name <- if (!is.null(pkg_path)) get_package_name(pkg_path) else NULL
  sidebar_config <- generate_sidebar_config(config, output_path, pkg_name)

  data <- list(
    title = config$site$title %||% "Package Documentation",
    use_katex = config$features$katex %||% TRUE,
    has_versions = has_version_support(config),
    has_logo = !is.null(config$site$logo),
    has_favicon = !is.null(config$site$favicon),
    has_github = !is.null(github_url),
    github_url = github_url %||% "",
    sidebar_config = sidebar_config
  )

  astro_config <- render_template("astro.config.mjs", data)

  writeLines(astro_config, file.path(output_path, "astro.config.mjs"))
  cli::cli_alert_success("Generated {.file astro.config.mjs}")
}

#' Generate package.json file
#'
#' @param output_path Path to output directory
#' @param config Configuration list
#' @param overwrite Logical, whether to overwrite existing file
#' @keywords internal
generate_package_json <- function(output_path, config, overwrite = FALSE) {
  package_json_path <- file.path(output_path, "package.json")

  # Don't overwrite existing package.json unless requested
  if (file.exists(package_json_path) && !overwrite) {
    cli::cli_alert_info("Skipping {.file package.json} (already exists)")
    return(invisible(NULL))
  }

  # Render base template (no whisker conditionals)
  package_json <- render_template("package.json", list())
  writeLines(package_json, package_json_path)

  # Merge additional dependencies into the rendered JSON
  merge_package_deps(package_json_path, config)

  cli::cli_alert_success("Generated {.file package.json}")
}

#' Merge katex and user dependencies into package.json
#'
#' @param package_json_path Path to the package.json file
#' @param config Configuration list
#' @keywords internal
merge_package_deps <- function(package_json_path, config) {
  pkg <- jsonlite::read_json(package_json_path)

  # Add katex dependency if enabled
  if (isTRUE(config$features$katex %||% TRUE)) {
    pkg$dependencies[["starlight-katex"]] <- "^0.0.4"
  }

  # Add user-managed dependencies from config
  if (!is.null(config$dependencies)) {
    for (dep in config$dependencies) {
      pkg$dependencies[[dep$name]] <- dep$version
    }
  }

  jsonlite::write_json(pkg, package_json_path, auto_unbox = TRUE, pretty = TRUE)
}

#' Generate content.config.ts file for Astro content collections
#'
#' @param output_path Path to output directory
#' @keywords internal
generate_content_config <- function(output_path) {
  src_path <- file.path(output_path, "src")
  copy_template("content.config.ts", file.path(src_path, "content.config.ts"))
  cli::cli_alert_success("Generated {.file content.config.ts}")
}

#' Generate .gitignore file for Starlight site
#'
#' @param output_path Path to output directory
#' @param overwrite Logical, whether to overwrite existing file
#' @keywords internal
generate_gitignore <- function(output_path, overwrite = FALSE) {
  gitignore_path <- file.path(output_path, ".gitignore")

  # Don't overwrite existing .gitignore unless requested
  if (file.exists(gitignore_path) && !overwrite) {
    cli::cli_alert_info("Skipping {.file .gitignore} (already exists)")
    return(invisible(NULL))
  }

  copy_template("gitignore", gitignore_path)
  cli::cli_alert_success("Generated {.file .gitignore}")
}

#' Generate remark-base-url.mjs plugin for versioned docs
#'
#' This remark plugin prepends ASTRO_BASE to image paths starting with /figures/
#' so that images work correctly in versioned documentation.
#'
#' @param output_path Path to output directory
#' @keywords internal
generate_remark_plugin <- function(output_path) {
  copy_template("remark-base-url.mjs", file.path(output_path, "remark-base-url.mjs"))
  cli::cli_alert_success("Generated {.file remark-base-url.mjs}")
}

#' Generate starlightr.css file for Starlight site
#'
#' Copies starlightr's own styles (argument tables, etc.) into the site.
#' This file is always overwritten because starlightr owns it.
#'
#' @param output_path Path to output directory
#' @keywords internal
generate_starlightr_css <- function(output_path) {
  template_path <- system.file("templates/starlightr.css", package = "starlightr")

  # Create styles directory
  styles_dir <- file.path(output_path, "src", "styles")
  if (!dir.exists(styles_dir)) {
    dir.create(styles_dir, recursive = TRUE)
  }

  css_path <- file.path(styles_dir, "starlightr.css")
  file.copy(template_path, css_path, overwrite = TRUE)
  cli::cli_alert_success("Generated {.file src/styles/starlightr.css}")
}

#' Generate custom.css file for Starlight site
#'
#' Creates a placeholder custom.css for user styles. Only created if missing;
#' this file is user-owned and never overwritten.
#'
#' @param output_path Path to output directory
#' @keywords internal
generate_custom_css <- function(output_path) {
  template_path <- system.file("templates/custom.css", package = "starlightr")

  # Create styles directory
  styles_dir <- file.path(output_path, "src", "styles")
  if (!dir.exists(styles_dir)) {
    dir.create(styles_dir, recursive = TRUE)
  }

  css_path <- file.path(styles_dir, "custom.css")

  if (file.exists(css_path)) {
    cli::cli_alert_info("Skipping {.file custom.css} (already exists)")
    return(invisible(NULL))
  }

  file.copy(template_path, css_path)
  cli::cli_alert_success("Generated {.file src/styles/custom.css}")
}
