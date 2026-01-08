# Asset handling for starlightr

#' Copy assets from package to site
#'
#' @param pkg_path Path to package directory
#' @param output_path Path to output directory
#' @param config Configuration list
#' @keywords internal
copy_assets <- function(pkg_path, output_path, config) {
  # Copy common assets if they exist
  assets_to_check <- c(
    "vignettes/assets",
    "vignettes/public",
    "man/figures"
  )

  public_dir <- file.path(output_path, "public")

  for (asset_path in assets_to_check) {
    full_path <- file.path(pkg_path, asset_path)
    if (dir.exists(full_path)) {
      file.copy(full_path, public_dir, recursive = TRUE)
      cli::cli_alert_success("Copied assets from {.path {asset_path}}")
    }
  }
}

#' Copy logo and favicon to site
#'
#' @param pkg_path Path to package directory
#' @param output_path Path to output directory
#' @param config Configuration list
#' @keywords internal
copy_branding_assets <- function(pkg_path, output_path, config) {
  # Copy logo to src/assets/ (Astro imports this as a module)
  if (!is.null(config$site$logo)) {
    logo_src <- file.path(pkg_path, config$site$logo)
    if (file.exists(logo_src)) {
      assets_dir <- file.path(output_path, "src", "assets")
      if (!dir.exists(assets_dir)) dir.create(assets_dir, recursive = TRUE)
      file.copy(logo_src, file.path(assets_dir, "logo.png"), overwrite = TRUE)
      cli::cli_alert_success("Copied logo to {.path src/assets/}")
    } else {
      cli::cli_warn("Logo file not found: {.path {logo_src}}")
    }
  }

  # Copy favicon to public/images/ (served as static file)
  if (!is.null(config$site$favicon)) {
    favicon_src <- file.path(pkg_path, config$site$favicon)
    if (file.exists(favicon_src)) {
      images_dir <- file.path(output_path, "public", "images")
      if (!dir.exists(images_dir)) dir.create(images_dir, recursive = TRUE)
      file.copy(favicon_src, file.path(images_dir, "favicon.png"), overwrite = TRUE)
      cli::cli_alert_success("Copied favicon to {.path public/images/}")
    } else {
      cli::cli_warn("Favicon file not found: {.path {favicon_src}}")
    }
  }
}
