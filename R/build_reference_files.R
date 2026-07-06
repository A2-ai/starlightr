#' Build reference MDX files from Rd files
#'
#' Self-contained function that captures example outputs, resolves external
#' links, and renders `.Rd` files to MDX. Example outputs (plots, tables, text)
#' are embedded inline in the MDX as base64 data URIs and raw HTML.
#'
#' @param rd_files Character vector of paths to `.Rd` files.
#' @param output_dir Path to directory where reference MDX files are saved.
#'   If relative, resolved against `pkg`.
#' @param pkg Path to the package directory (default `"."`).
#' @param config_file Path to `_starlightr.toml` (relative to `pkg`).
#' @param examples Logical, whether to capture and embed example outputs
#'   (default `TRUE`).
#' @param verbose Logical, whether to print debug messages during example
#'   capture (default `FALSE`).
#'
#' @return Invisibly returns a character vector of written file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' # Build specific reference files
#' build_reference_files(
#'   c("man/build_site.Rd", "man/add_article.Rd"),
#'   output_dir = "../my-site/src/content/docs/reference"
#' )
#'
#' # Build without examples
#' build_reference_files(
#'   "man/build_site.Rd",
#'   output_dir = "/tmp/ref",
#'   examples = FALSE
#' )
#' }
build_reference_files <- function(
  rd_files,
  output_dir,
  pkg = ".",
  config_file = "_starlightr.toml",
  examples = TRUE,
  verbose = FALSE
) {
  pkg_path <- normalizePath(pkg, mustWork = TRUE)
  config_path <- file.path(pkg_path, config_file)

  # A relative output_dir must resolve against the package root, not
  # whatever directory render_reference()'s Rust side falls back to
  # (std::env::current_dir()) — see resolve_against(). Always cross the
  # R/Rust boundary with an absolute path.
  output_dir <- resolve_against(output_dir, pkg_path)

  # The Rust emitter re-reads its own config file rather than accepting
  # options in-process; give it R's merged config (user values over
  # default_config()) instead of the raw file, so `[reference]` defaults
  # (skip_sections, section_order) actually apply. See config.R.
  config <- read_config(config_path)
  emit_config_path <- write_reference_config_toml(config$reference)

  # Validate rd_files exist
  rd_files <- normalizePath(rd_files, mustWork = FALSE)
  existing <- file.exists(rd_files)
  if (any(!existing)) {
    for (f in rd_files[!existing]) {
      cli::cli_warn("Rd file not found: {.file {f}}")
    }
  }
  rd_files <- rd_files[existing]
  if (length(rd_files) == 0) {
    cli::cli_warn("No Rd files to process")
    return(invisible(character()))
  }

  ensure_dir(output_dir)

  # Capture examples inline
  example_outputs_file <- NULL
  if (examples) {
    pkg_name <- get_package_name(pkg_path)
    fn_names <- tools::file_path_sans_ext(basename(rd_files))

    cli::cli_alert_info("Capturing example outputs...")
    captured <- capture_rd_examples(pkg_name, fn_names, verbose = verbose)

    if (length(captured) > 0) {
      example_outputs_file <- build_inline_example_outputs_map(captured)
      cli::cli_alert_success("Captured examples for {length(captured)} function{?s}")
    }
  }

  # Build external link map
  dep_packages <- extract_dependency_packages(pkg_path)
  external_links_file <- build_external_link_map(dep_packages)

  # Render each Rd file. A single malformed Rd must not abort the whole
  # batch (and the site build with it) — collect failures and keep going,
  # then report a summary. This mirrors the warn-and-skip policy used for
  # example capture in capture_rd_examples().
  cli::cli_alert_info("Rendering {length(rd_files)} reference file{?s}...")
  rendered <- character()
  render_failures <- character()
  for (rd_file in rd_files) {
    ok <- tryCatch(
      {
        render_reference(
          rd_file = rd_file,
          output_dir = output_dir,
          config_file = emit_config_path,
          external_links_file = external_links_file,
          example_outputs_file = example_outputs_file
        )
        TRUE
      },
      error = function(e) {
        render_failures <<- c(
          render_failures,
          sprintf("%s: %s", rd_file, conditionMessage(e))
        )
        FALSE
      }
    )
    if (ok) {
      rendered <- c(rendered, rd_file)
    }
  }

  written_files <- file.path(
    output_dir,
    paste0(vapply(rendered, rd_file_to_slug, character(1)), ".mdx")
  )

  if (length(render_failures) > 0) {
    bullets <- render_failures
    names(bullets) <- rep_len("x", length(bullets))
    cli::cli_warn(c(
      "Skipped {length(render_failures)} reference file{?s} due to render errors:",
      bullets
    ))
  }

  cli::cli_alert_success(
    "Wrote {length(written_files)} reference file{?s} to {.path {output_dir}}"
  )
  invisible(written_files)
}

#' Build all reference MDX files for a package
#'
#' Convenience wrapper around [build_reference_files()] that processes all
#' `.Rd` files in the package's `man/` directory.
#'
#' @inheritParams build_reference_files
#' @param include_internal Logical, whether to include internal functions
#'   (those with `@keywords internal`). If `NULL` (default), reads from
#'   `reference.include_internal` in `_starlightr.toml`, falling back to
#'   `FALSE`.
#'
#' @return Invisibly returns a character vector of written file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' build_package_reference_docs(
#'   output_dir = "../my-site/src/content/docs/reference"
#' )
#'
#' # Include internal functions (overrides config)
#' build_package_reference_docs(
#'   output_dir = "../my-site/src/content/docs/reference",
#'   include_internal = TRUE
#' )
#' }
build_package_reference_docs <- function(
  output_dir,
  pkg = ".",
  config_file = "_starlightr.toml",
  examples = TRUE,
  include_internal = NULL,
  verbose = FALSE
) {
  pkg_path <- normalizePath(pkg, mustWork = TRUE)
  rd_dir <- file.path(pkg_path, "man")

  if (!dir.exists(rd_dir)) {
    cli::cli_abort("No {.path man/} directory found in {.path {pkg_path}}")
  }

  # Resolve include_internal: arg overrides config, config overrides FALSE
  if (is.null(include_internal)) {
    config_path <- file.path(pkg_path, config_file)
    if (file.exists(config_path)) {
      config <- read_config(config_path)
      include_internal <- config$reference$include_internal %||% FALSE
    } else {
      include_internal <- FALSE
    }
  }

  rd_files <- list.files(rd_dir, pattern = "\\.Rd$", full.names = TRUE)

  if (!include_internal) {
    rd_files <- Filter(function(f) {
      content <- readLines(f, warn = FALSE)
      !any(grepl("\\\\keyword\\{internal\\}", content))
    }, rd_files)
  }

  if (length(rd_files) == 0) {
    cli::cli_warn("No Rd files found in {.path {rd_dir}}")
    return(invisible(character()))
  }

  build_reference_files(
    rd_files = rd_files,
    output_dir = output_dir,
    pkg = pkg,
    config_file = config_file,
    examples = examples,
    verbose = verbose
  )
}

#' Build inline example outputs JSON map
#'
#' Takes in-memory captured results from [capture_rd_examples()] and produces
#' a temporary JSON file with inline content (base64 PNGs, raw HTML, text).
#'
#' @param captured Named list from `capture_rd_examples()`
#' @return Path to temporary JSON file
#' @keywords include_internal
#' @noRd
build_inline_example_outputs_map <- function(captured) {
  outputs <- list()

  for (fn_name in names(captured)) {
    entry <- list()
    cap <- captured[[fn_name]]

    if (!is.null(cap$txt) && nchar(cap$txt) > 0) {
      entry$txt <- cap$txt
    }

    if (!is.null(cap$png_raw)) {
      data_uris <- vapply(
        cap$png_raw,
        function(png_raw) {
          paste0("data:image/png;base64,", base64enc::base64encode(png_raw))
        },
        character(1)
      )
      # I() forces array serialization even when there's exactly one
      # element -- otherwise auto_unbox would collapse it to a bare string
      # and the Rust side (which always expects an array) would fail to parse.
      entry$png <- I(unname(data_uris))
    }

    if (!is.null(cap$html)) {
      entry$html <- I(unname(vapply(cap$html, as.character, character(1))))
    }

    if (length(entry) > 0) {
      outputs[[fn_name]] <- entry
    }
  }

  json_path <- tempfile("starlightr-examples-", fileext = ".json")
  writeLines(jsonlite::toJSON(outputs, auto_unbox = TRUE), json_path)
  json_path
}

rd_file_to_slug <- function(path) {
  stem <- tools::file_path_sans_ext(basename(path))
  slugify(stem)
}
