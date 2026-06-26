# Version support functions for starlightr

#' Check if version support is enabled
#'
#' @param config Configuration list
#' @return Logical indicating whether version support is enabled
#' @keywords internal
#' @noRd
has_version_support <- function(config) {
  isTRUE(config$versions$enabled)
}

#' Get current version from environment or config
#'
#' Priority: STARLIGHTR_VERSION env var > config$versions$current > "dev"
#'
#' @param config Configuration list
#' @return Character string with current version
#' @keywords internal
#' @noRd
get_current_version <- function(config) {
  env_version <- Sys.getenv("STARLIGHTR_VERSION", unset = NA)
  if (!is.na(env_version) && nchar(env_version) > 0) {
    return(env_version)
  }

  config$versions$current %||% "dev"
}

#' Validate version configuration
#'
#' @param config Configuration list
#' @return TRUE if valid, otherwise throws error
#' @keywords internal
#' @noRd
validate_version_config <- function(config) {
  if (!has_version_support(config)) {
    return(TRUE)
  }

  versions <- config$versions$list

  if (is.null(versions) || length(versions) == 0) {
    cli::cli_abort(
      "versions.list must contain at least one version when versions.enabled is true"
    )
  }

  # Check for exactly one default
  defaults <- vapply(versions, function(v) isTRUE(v$default), logical(1))
  if (sum(defaults) == 0) {
    cli::cli_warn(
      "No default version specified. First version will be used as default."
    )
  } else if (sum(defaults) > 1) {
    cli::cli_abort("Only one version can have default: true")
  }

  # Check for duplicate tags
  tags <- vapply(versions, function(v) v$tag %||% "", character(1))
  tags <- tags[nchar(tags) > 0]
  if (anyDuplicated(tags)) {
    cli::cli_abort("Duplicate tags found in versions.list")
  }

  TRUE
}

#' Generate versions.ts data file
#'
#' @param output_path Path to output directory
#' @param config Configuration list
#' @keywords internal
#' @noRd
generate_versions_ts <- function(output_path, config) {
  # Prepare data for whisker
  versions_data <- lapply(config$versions$list, function(v) {
    list(
      tag = v$tag %||% "",
      label = v$label %||% v$tag %||% "",
      default = isTRUE(v$default)
    )
  })

  data <- list(
    versions = versions_data,
    currentVersion = get_current_version(config)
  )

  rendered <- render_template("versions.ts", data)

  # Ensure output directory exists
  data_dir <- file.path(output_path, "src", "data")
  ensure_dir(data_dir)

  writeLines(rendered, file.path(data_dir, "versions.ts"))
  cli::cli_alert_success("Generated {.file src/data/versions.ts}")
}

#' Generate VersionSelect.astro component
#'
#' @param output_path Path to output directory
#' @param config Configuration list
#' @keywords internal
#' @noRd
generate_version_select_component <- function(output_path, config) {
  template_path <- system.file(
    "templates/VersionSelect.astro",
    package = "starlightr"
  )

  # Ensure output directory exists
  components_dir <- file.path(output_path, "src", "components")
  ensure_dir(components_dir)

  # Copy template directly (no templating needed)
  file.copy(
    template_path,
    file.path(components_dir, "VersionSelect.astro"),
    overwrite = TRUE
  )
  cli::cli_alert_success("Generated {.file src/components/VersionSelect.astro}")
}

#' Generate deploy-docs.yml GitHub Actions workflow
#'
#' @param output_path Path to docs output directory
#' @param config Configuration list
#' @param overwrite Logical, whether to overwrite existing file
#' @keywords internal
#' @noRd
generate_deploy_workflow <- function(output_path, config, overwrite = FALSE) {
  template_path <- system.file(
    "templates/deploy-docs.yml",
    package = "starlightr"
  )

  # Create .github/workflows directory in docs output
  workflows_dir <- file.path(output_path, ".github", "workflows")
  ensure_dir(workflows_dir)

  workflow_path <- file.path(workflows_dir, "deploy-docs.yml")

  # Don't overwrite existing workflow unless requested
  if (file.exists(workflow_path) && !overwrite) {
    cli::cli_alert_info(
      "Skipping {.file .github/workflows/deploy-docs.yml} (already exists)"
    )
    return(invisible(NULL))
  }

  file.copy(template_path, workflow_path, overwrite = TRUE)
  cli::cli_alert_success("Generated {.file .github/workflows/deploy-docs.yml}")
}
