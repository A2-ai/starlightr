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

  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
}

#' Generate Starlight configuration files
#'
#' @param output_path Path to output directory
#' @param config Configuration list
#' @param pkg_path Path to package directory
#' @keywords internal
generate_starlight_config <- function(output_path, config, pkg_path) {
  # Generate astro.config.mjs
  generate_astro_config(output_path, config, pkg_path)

  # Generate content.config.ts (required for Astro content collections)
  generate_content_config(output_path)

  # Generate package.json if requested
  if (config$output$include_build_files %||% TRUE) {
    generate_package_json(output_path, config)
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
  social_config <- ""

  if (!is.null(github_url)) {
    social_config <- sprintf("social: [{ icon: 'github', label: 'GitHub', href: '%s' }],", github_url)
  }

  # Logo configuration
  logo_config <- ""
  if (!is.null(config$site$logo)) {
    logo_config <- 'logo: { src: "./src/assets/logo.png", alt: "Logo" },'
  }

  # Favicon configuration
  favicon_config <- ""
  if (!is.null(config$site$favicon)) {
    favicon_config <- 'favicon: "/images/favicon.png",'
  }

  # KaTeX support (enabled by default for R packages with math)
  use_katex <- config$features$katex %||% TRUE
  katex_import <- ""
  katex_plugin <- ""
  if (use_katex) {
    katex_import <- 'import { starlightKatex } from "starlight-katex";'
    katex_plugin <- "plugins: [starlightKatex()],"
  }

  # Generate sidebar configuration from YAML
  pkg_name <- if (!is.null(pkg_path)) get_package_name(pkg_path) else NULL
  sidebar_config <- generate_sidebar_config(config, output_path, pkg_name)

  astro_config <- sprintf('// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
%s

// https://astro.build/config
export default defineConfig({
  integrations: [
    starlight({
      title: "%s",
      %s
      %s
      %s
      %s
      sidebar: %s
    })
  ]
});
',
    katex_import,
    config$site$title %||% "Package Documentation",
    katex_plugin,
    logo_config,
    favicon_config,
    social_config,
    sidebar_config
  )

  writeLines(astro_config, file.path(output_path, "astro.config.mjs"))
  cli::cli_alert_success("Generated {.file astro.config.mjs}")
}

#' Generate package.json file
#'
#' @param output_path Path to output directory
#' @param config Configuration list
#' @keywords internal
generate_package_json <- function(output_path, config) {
  package_json_path <- file.path(output_path, "package.json")

  # Don't overwrite existing package.json
  if (file.exists(package_json_path)) {
    cli::cli_alert_info("Skipping {.file package.json} (already exists)")
    return(invisible(NULL))
  }

  # Check if KaTeX is enabled (default TRUE)
  use_katex <- config$features$katex %||% TRUE

  katex_dep <- ""
  if (use_katex) {
    katex_dep <- ',
    "starlight-katex": "^0.0.4"'
  }

  package_json <- sprintf('{
  "name": "starlight-docs",
  "type": "module",
  "version": "0.0.1",
  "scripts": {
    "dev": "astro dev",
    "start": "astro dev",
    "build": "astro build",
    "preview": "astro preview",
    "astro": "astro"
  },
  "dependencies": {
    "@astrojs/starlight": "^0.36.0",
    "astro": "^5.6.1",
    "sharp": "^0.34.2"%s
  }
}', katex_dep)

  writeLines(package_json, package_json_path)
  cli::cli_alert_success("Generated {.file package.json}")
}

#' Generate content.config.ts file for Astro content collections
#'
#' @param output_path Path to output directory
#' @keywords internal
generate_content_config <- function(output_path) {
  content_config <- 'import { defineCollection } from "astro:content";
import { docsLoader } from "@astrojs/starlight/loaders";
import { docsSchema } from "@astrojs/starlight/schema";

export const collections = {
  docs: defineCollection({ loader: docsLoader(), schema: docsSchema() }),
};
'

  src_path <- file.path(output_path, "src")
  writeLines(content_config, file.path(src_path, "content.config.ts"))
  cli::cli_alert_success("Generated {.file content.config.ts}")
}
