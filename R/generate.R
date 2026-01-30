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

  # Version selector component override
  components_config <- ""
  if (has_version_support(config)) {
    components_config <- 'components: { SiteTitle: "./src/components/VersionSelect.astro" },'
  }

  # Generate sidebar configuration from YAML
  pkg_name <- if (!is.null(pkg_path)) get_package_name(pkg_path) else NULL
  sidebar_config <- generate_sidebar_config(config, output_path, pkg_name)

  astro_config <- sprintf('// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import { remarkBaseUrl } from "./remark-base-url.mjs";
%s

// https://astro.build/config
export default defineConfig({
  site: process.env.ASTRO_SITE || "http://localhost",
  base: process.env.ASTRO_BASE || "/",
  trailingSlash: "always",
  markdown: {
    remarkPlugins: [remarkBaseUrl],
  },
  integrations: [
    starlight({
      title: "%s",
      customCss: ["./src/styles/custom.css"],
      %s
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
    components_config,
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
#' @param overwrite Logical, whether to overwrite existing file
#' @keywords internal
generate_package_json <- function(output_path, config, overwrite = FALSE) {
  package_json_path <- file.path(output_path, "package.json")

  # Don't overwrite existing package.json unless requested
  if (file.exists(package_json_path) && !overwrite) {
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
    "sharp": "^0.34.2",
    "unist-util-visit": "^5.0.0"%s
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

  gitignore_content <- "# build output
dist/
# generated types
.astro/

# dependencies
node_modules/

# logs
npm-debug.log*
yarn-debug.log*
yarn-error.log*
pnpm-debug.log*

# environment variables
.env
.env.production

# macOS-specific files
.DS_Store
"

  writeLines(gitignore_content, gitignore_path)
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
  plugin_content <- 'import { visit } from "unist-util-visit";

/**
 * Remark plugin to prepend ASTRO_BASE to image paths.
 * This ensures images work correctly in versioned documentation.
 */
export function remarkBaseUrl() {
  const base = (process.env.ASTRO_BASE || "/").replace(/\\/$/, "");

  return (tree) => {
    visit(tree, "image", (node) => {
      if (node.url && node.url.startsWith("/figures/")) {
        node.url = base + node.url;
      }
    });
  };
}
'

  writeLines(plugin_content, file.path(output_path, "remark-base-url.mjs"))
  cli::cli_alert_success("Generated {.file remark-base-url.mjs}")
}

#' Generate custom.css file for Starlight site
#'
#' @param output_path Path to output directory
#' @param overwrite Logical, whether to overwrite existing file
#' @keywords internal
generate_custom_css <- function(output_path, overwrite = FALSE) {
  template_path <- system.file("templates/custom.css", package = "starlightr")

  # Create styles directory
  styles_dir <- file.path(output_path, "src", "styles")
  if (!dir.exists(styles_dir)) {
    dir.create(styles_dir, recursive = TRUE)
  }

  css_path <- file.path(styles_dir, "custom.css")

  # Don't overwrite existing custom.css unless requested
  if (file.exists(css_path) && !overwrite) {
    cli::cli_alert_info("Skipping {.file custom.css} (already exists)")
    return(invisible(NULL))
  }

  file.copy(template_path, css_path)
  cli::cli_alert_success("Generated {.file src/styles/custom.css}")
}
