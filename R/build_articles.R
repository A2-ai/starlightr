#' Build article markdown files from Rmd sources
#'
#' Renders `.Rmd` files to markdown and post-processes them for use in a
#' Starlight site. Titles are read from each file's YAML frontmatter.
#' Figures are embedded inline as base64 data URIs so only `output_dir`
#' is needed.
#'
#' @param rmd_files Character vector of paths to `.Rmd` (or `.md`) files.
#' @param output_dir Path to directory where article `.md` files are saved.
#' @param verbose Logical, whether to print debug messages during Rmd
#'   rendering (default `FALSE`).
#'
#' @return Invisibly returns a character vector of written file paths.
#' @export
#'
#' @examples
#' \dontrun{
#' build_articles(
#'   c("vignettes/introduction.Rmd", "README.Rmd"),
#'   output_dir = "../my-site/src/content/docs/articles"
#' )
#' }
build_articles <- function(
  rmd_files,
  output_dir,
  verbose = FALSE
) {
  ensure_dir(output_dir)

  # Validate existence
  rmd_files <- normalizePath(rmd_files, mustWork = FALSE)
  existing <- file.exists(rmd_files)
  if (any(!existing)) {
    for (f in rmd_files[!existing]) {
      cli::cli_warn("File not found: {.file {f}}")
    }
  }
  rmd_files <- rmd_files[existing]

  if (length(rmd_files) == 0) {
    cli::cli_alert_info("No article files found")
    return(invisible(character()))
  }

  # Read titles from YAML frontmatter before rendering
  titles <- vapply(rmd_files, read_rmd_title, character(1))

  # Split into Rmd (need building) and pre-rendered Md (just copy)
  is_rmd <- grepl("\\.Rmd$", rmd_files, ignore.case = TRUE)
  rmd_to_build <- rmd_files[is_rmd]
  md_to_copy <- rmd_files[!is_rmd]

  # Render each Rmd in a clean callr subprocess. `pkgload::load_all` in
  # the subprocess loads the package from source (same as an interactive
  # `devtools::load_all()`), so we don't need to install. Imports resolve
  # from the caller's user library, inherited via `.libPaths()`.
  # `output_dir` relocates outputs into the tempdir, leaving the source
  # `vignettes/` untouched on a successful render.
  build_dir <- tempfile("starlightr-rmd-")
  ensure_dir(build_dir)
  on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)

  if (length(rmd_to_build) > 0) {
    cli::cli_alert_info("Building {length(rmd_to_build)} Rmd file{?s}...")
    pkg_path <- normalizePath(".", mustWork = TRUE)
    for (rmd in rmd_to_build) {
      cli::cli_inform(c(i = "Building {.path {rmd}}"))
      callr::r_safe(
        function(pkg_path, input, output_format, output_dir, quiet) {
          pkgload::load_all(pkg_path, quiet = quiet, helpers = FALSE)
          rmarkdown::render(
            input = input,
            output_format = output_format,
            output_dir = output_dir,
            quiet = quiet
          )
        },
        args = list(
          pkg_path = pkg_path,
          input = rmd,
          output_format = rmarkdown::md_document(
            variant = "gfm",
            preserve_yaml = FALSE
          ),
          output_dir = build_dir,
          quiet = !verbose
        ),
        show = TRUE,
        spinner = FALSE,
        stderr = "2>&1"
      )
    }
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
  for (i in seq_along(rmd_files)) {
    src <- rmd_files[i]
    md_name <- tools::file_path_sans_ext(basename(src))
    slug <- slugify(md_name)
    title <- titles[i]

    out_file <- process_article_inline(
      slug,
      md_name,
      build_dir,
      output_dir,
      title
    )
    if (!is.null(out_file)) {
      written_files <- c(written_files, out_file)
    }
  }

  cli::cli_alert_success("Generated {length(written_files)} article{?s}")
  invisible(written_files)
}

#' Build all articles for a package
#'
#' Discovers all vignettes and README in the package and builds them.
#'
#' @param output_dir Path to directory where article `.md` files are saved.
#' @param pkg Path to the package directory (default `"."`).
#' @param verbose Logical, whether to print debug messages during Rmd
#'   rendering (default `FALSE`).
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
  verbose = FALSE
) {
  pkg_path <- normalizePath(pkg, mustWork = TRUE)

  # Discover vignettes
  vignettes_dir <- file.path(pkg_path, "vignettes")
  rmd_files <- character()
  if (dir.exists(vignettes_dir)) {
    rmd_files <- list.files(
      vignettes_dir,
      pattern = "\\.Rmd$",
      full.names = TRUE
    )
  }

  # Discover README
  readme_path <- find_readme(pkg_path)
  if (!is.null(readme_path)) {
    rmd_files <- c(readme_path, rmd_files)
  }

  if (length(rmd_files) == 0) {
    cli::cli_alert_info("No vignettes or README found in {.path {pkg_path}}")
    return(invisible(character()))
  }

  build_articles(
    rmd_files = rmd_files,
    output_dir = output_dir,
    verbose = verbose
  )
}

#' Read title from Rmd/md YAML frontmatter
#'
#' @param path Path to .Rmd or .md file
#' @return Title string, or a fallback derived from filename
#' @keywords internal
read_rmd_title <- function(path) {
  lines <- readLines(path, n = 20, warn = FALSE)

  # Check for YAML frontmatter
  if (length(lines) == 0 || lines[1] != "---") {
    return(tools::toTitleCase(gsub("[-_]", " ", tools::file_path_sans_ext(basename(path)))))
  }

  end <- which(lines[-1] == "---")[1]
  if (is.na(end)) {
    return(tools::toTitleCase(gsub("[-_]", " ", tools::file_path_sans_ext(basename(path)))))
  }

  yaml_block <- lines[2:end]
  parsed <- tryCatch(
    yaml::yaml.load(paste(yaml_block, collapse = "\n")),
    error = function(e) NULL
  )

  if (!is.null(parsed$title)) {
    return(parsed$title)
  }

  tools::toTitleCase(gsub("[-_]", " ", tools::file_path_sans_ext(basename(path))))
}

#' Process a single article's built output with inline figures
#'
#' Post-processes a built markdown file: inlines figures as base64 data URIs,
#' fixes paths, and adds YAML frontmatter.
#'
#' @param slug Output file slug (e.g., "readme", "introduction")
#' @param md_name Name of the .md file without extension
#' @param source_dir Directory containing the built .md and figure files
#' @param output_dir Directory where final .md is written
#' @param title Title for the article frontmatter
#' @return Path to written file, or NULL if source not found
#' @keywords internal
process_article_inline <- function(
  slug,
  md_name,
  source_dir,
  output_dir,
  title
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

  final_content <- paste0(
    "---\ntitle: \"",
    title,
    "\"\npagefind: true\n---\n\n",
    md_content
  )

  out_file <- file.path(output_dir, paste0(slug, ".md"))
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

    # Replace the path in markdown
    md_content <- gsub(rel_path, data_uri, md_content, fixed = TRUE)
  }

  # Handle temp directory figure paths that include the full temp path
  md_content <- gsub(
    "(!\\[[^]]*\\]\\()[^)]*starlightr-rmd-[^)]+\\)",
    "\\1)",
    md_content,
    perl = TRUE
  )

  md_content
}
