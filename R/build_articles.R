#' Build article markdown files from Rmd sources
#'
#' Renders `.Rmd` files to markdown and post-processes them for use in a
#' Starlight site. Titles are read from each file's YAML frontmatter.
#' Figures are copied into the site's `public/figures/` directory and
#' referenced with `/figures/...` paths, keeping the generated `.md`
#' source readable and letting Astro optimize the images.
#'
#' @param rmd_files Character vector of paths to `.Rmd` (or `.md`) files.
#' @param output_dir Path to directory where article `.md` files are saved.
#' @param site_dir Path to the Starlight site root, where figures are copied
#'   under `public/figures/`. If `NULL` (default), it is derived from
#'   `output_dir` by stripping the trailing `src/content/docs/...` segment.
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
#'   output_dir = "../my-site/src/content/docs/articles",
#'   site_dir = "../my-site"
#' )
#' }
build_articles <- function(
  rmd_files,
  output_dir,
  site_dir = NULL,
  verbose = FALSE
) {
  ensure_dir(output_dir)

  if (is.null(site_dir)) {
    site_dir <- resolve_site_dir(output_dir)
  }
  public_figures_dir <- file.path(site_dir, "public", "figures")

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
      title,
      public_figures_dir,
      rmd_dir = dirname(src)
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
#' @param site_dir Path to the Starlight site root, where figures are copied
#'   under `public/figures/`. If `NULL` (default), it is derived from
#'   `output_dir`.
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
  site_dir = NULL,
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
    site_dir = site_dir,
    verbose = verbose
  )
}

#' Read title from Rmd/md YAML frontmatter
#'
#' @param path Path to .Rmd or .md file
#' @return Title string, or a fallback derived from filename
#' @keywords internal
#' @noRd
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
#' Post-processes a built markdown file: copies figures into the site's
#' `public/figures/` directory, fixes paths, and adds YAML frontmatter.
#'
#' @param slug Output file slug (e.g., "readme", "introduction")
#' @param md_name Name of the .md file without extension
#' @param source_dir Directory containing the built .md and figure files
#' @param output_dir Directory where final .md is written
#' @param title Title for the article frontmatter
#' @param public_figures_dir Site `public/figures/` directory where figures
#'   are copied. Files are placed under a per-slug subdirectory.
#' @param rmd_dir Directory of the source Rmd. Used to locate figures
#'   written under the README convention (`man/figures/<prefix>-*`).
#' @return Path to written file, or NULL if source not found
#' @keywords internal
#' @noRd
process_article_inline <- function(
  slug,
  md_name,
  source_dir,
  output_dir,
  title,
  public_figures_dir,
  rmd_dir = NULL
) {
  md_file <- file.path(source_dir, paste0(md_name, ".md"))
  if (!file.exists(md_file)) {
    cli::cli_warn("Built markdown not found: {.file {md_file}}")
    return(NULL)
  }

  md_content <- paste(readLines(md_file, warn = FALSE), collapse = "\n")

  # rmarkdown can emit image paths as absolutes under our build dir:
  # `![](/private/var/.../starlightr-rmd-XXX/<name>_files/figure-gfm/foo.png)`.
  # Strip the absolute prefix down to the part after the build-dir segment so
  # the inline lookup (keyed on relative paths like `<name>_files/...`) matches.
  md_content <- gsub(
    "(!\\[[^]]*\\]\\()[^)]*/starlightr-rmd-[^/]+/",
    "\\1",
    md_content,
    perl = TRUE
  )

  # Collect all figure files from known locations
  figure_files <- collect_article_figures(md_name, source_dir, rmd_dir)

  # Copy figures into public/figures/<slug>/ and rewrite to /figures/ paths
  md_content <- copy_figure_references(
    md_content,
    figure_files,
    public_figures_dir,
    slug
  )

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
#' @param rmd_dir Directory of the source Rmd. When supplied, figures
#'   under `<rmd_dir>/man/figures/` are also collected (README convention:
#'   `knitr::opts_chunk$set(fig.path = "man/figures/README-")`).
#' @return Named list mapping relative paths (as they appear in markdown) to
#'   absolute file paths
#' @keywords internal
#' @noRd
collect_article_figures <- function(md_name, source_dir, rmd_dir = NULL) {
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

  # README convention: figures written under <rmd_dir>/man/figures/
  if (!is.null(rmd_dir)) {
    man_figures <- file.path(rmd_dir, "man", "figures")
    if (dir.exists(man_figures)) {
      files <- list.files(man_figures, full.names = TRUE)
      files <- files[!dir.exists(files)]
      for (f in files) {
        rel_path <- file.path("man/figures", basename(f))
        figures[[rel_path]] <- f
      }
    }
  }

  figures
}

#' Copy figure files into the site's public directory and rewrite references
#'
#' Finds markdown image references (`![alt](path)`) and replaces local file
#' paths with `/figures/<slug>/<file>` paths after copying each figure into
#' `<public_figures_dir>/<slug>/`. The `/figures/` prefix matches the
#' `remark-base-url.mjs` plugin, which prepends the deployment base URL.
#'
#' @param md_content Markdown string
#' @param figure_files Named list from `collect_article_figures()`
#' @param public_figures_dir Site `public/figures/` directory
#' @param slug Article slug, used as a per-article subdirectory
#' @return Markdown with `/figures/...` image references
#' @keywords internal
#' @noRd
copy_figure_references <- function(md_content, figure_files, public_figures_dir, slug) {
  dest_dir <- file.path(public_figures_dir, slug)

  for (rel_path in names(figure_files)) {
    abs_path <- figure_files[[rel_path]]
    if (!file.exists(abs_path)) next

    ensure_dir(dest_dir)
    file_name <- basename(abs_path)
    file.copy(abs_path, file.path(dest_dir, file_name), overwrite = TRUE)

    web_path <- paste0("/figures/", slug, "/", file_name)

    # Replace the path in markdown
    md_content <- gsub(rel_path, web_path, md_content, fixed = TRUE)
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
