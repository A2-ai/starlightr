# Deploy workflow functions for starlightr

#' Check if a deploy workflow should be generated
#'
#' `deploy.workflow` decides when set. The deprecated `versions.workflow` is
#' still honoured for versioned sites. Otherwise versioned sites get a
#' workflow and unversioned sites do not.
#'
#' @param config Configuration list
#' @return Logical
#' @keywords internal
#' @noRd
wants_deploy_workflow <- function(config) {
  if (!is.null(config$deploy$workflow)) {
    return(isTRUE(config$deploy$workflow))
  }

  if (!is.null(config$versions$workflow)) {
    cli::cli_warn(
      "{.field versions.workflow} is deprecated, use {.field deploy.workflow} instead."
    )
    return(has_version_support(config) && !isFALSE(config$versions$workflow))
  }

  has_version_support(config)
}

#' Find the root of the git repository containing a path
#'
#' @param path Path to start from
#' @return The repository root, or NULL when `path` is not in a repository
#' @keywords internal
#' @noRd
find_git_root <- function(path) {
  dir <- normalizePath(path, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(dir, ".git"))) {
      return(dir)
    }
    parent <- dirname(dir)
    if (parent == dir) {
      return(NULL)
    }
    dir <- parent
  }
}

#' Generate deploy-docs.yml GitHub Actions workflow
#'
#' The workflow is written to `.github/workflows/` at the root of the git
#' repository containing the site, so the site can be its own repository or
#' a subdirectory of the package repository. A site outside any repository
#' is treated as the root of its own.
#'
#' @param output_path Path to docs output directory
#' @param config Configuration list
#' @param overwrite Logical, whether to overwrite existing file
#' @keywords internal
#' @noRd
generate_deploy_workflow <- function(output_path, config, overwrite = FALSE) {
  site_path <- normalizePath(output_path, winslash = "/", mustWork = TRUE)
  repo_root <- find_git_root(site_path) %||% site_path
  docs_dir <- if (site_path == repo_root) {
    "."
  } else {
    substring(site_path, nchar(repo_root) + 2)
  }

  template <- if (has_version_support(config)) {
    "deploy-docs.yml"
  } else {
    "deploy-docs-unversioned.yml"
  }

  workflows_dir <- file.path(repo_root, ".github", "workflows")
  ensure_dir(workflows_dir)

  workflow_path <- file.path(workflows_dir, "deploy-docs.yml")

  # Don't overwrite existing workflow unless requested
  if (file.exists(workflow_path) && !overwrite) {
    cli::cli_alert_info(
      "Skipping {.path {workflow_path}} (already exists)"
    )
    return(invisible(NULL))
  }

  # JSON strings are valid YAML scalars; render them without HTML escaping.
  data <- list(
    docs_dir = as.character(jsonlite::toJSON(docs_dir, auto_unbox = TRUE)),
    publish_dir = as.character(jsonlite::toJSON(
      paste0(docs_dir, "/dist"),
      auto_unbox = TRUE
    ))
  )
  writeLines(render_template(template, data), workflow_path)
  cli::cli_alert_success("Generated {.path {workflow_path}}")
}
