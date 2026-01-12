
<!-- README.md is generated from README.Rmd. Please edit that file -->

# starlightr

<!-- badges: start -->

[![R-CMD-check](https://github.com/A2-ai/starlightr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/A2-ai/starlightr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**starlightr** generates beautiful, modern documentation sites for R
packages using [Starlight](https://starlight.astro.build/) (built on
[Astro](https://astro.build/)). Think of it as an alternative to pkgdown
that produces fast, accessible documentation with a polished UI out of
the box.

## Features

- Converts Rd documentation to clean Markdown/MDX
- Processes vignettes into articles
- Generates Astro/Starlight configuration automatically
- Supports lifecycle badges, code examples, and complex documentation
- **Example output embedding** - automatically captures and displays
  plot outputs, text results, and HTML widgets
- **Multi-version documentation** - deploy multiple versions with a
  version selector dropdown
- **KaTeX math support** - render LaTeX equations out of the box
- Produces static sites that can be hosted anywhere (GitHub Pages,
  Netlify, Vercel, Cloudflare Pages, etc.)

## Installation

``` r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("A2-ai/starlightr")
```

### Requirements

- **R \>= 4.0**
- **pandoc** (comes with RStudio, or install separately)
- **Node.js \>= 18** (for building/previewing the Starlight site)

## Quick Start

### 1. Initialize starlightr in your package

``` r
library(starlightr)

# Run from your package directory
use_starlightr()
```

This creates: - `_starlightr.yaml` - configuration file - Updates
`.Rbuildignore`

### 2. Build the documentation site

``` r
# Build to an external directory (recommended)
build_site(output_dir = "../my-package-docs")

# Or build to a subdirectory (adds bloat to your package)
build_site(output_dir = "docs")
```

Note: your package should be installed and up to date before running
`build_site()` so starlightr can read Rd documentation and capture
example outputs.

### 3. Preview your site

``` bash
cd ../my-package-docs  # or wherever you built to
npm install
npm run dev
```

Open <http://localhost:4321> to see your documentation!

## Configuration

The `_starlightr.yaml` file controls how your site is generated:

``` yaml
# Site metadata
site:
  title: "My Package"
  description: "A brief description of your package"
  github: "https://github.com/user/repo"  # Adds GitHub link to header
  logo: "man/figures/logo.png"            # Optional logo
  favicon: "man/figures/favicon.png"      # Optional favicon

# Homepage hero section
home:
  hero:
    tagline: "What your package does in one sentence"
    actions:
      - text: "Get Started"
        link: "/articles/"
        icon: "right-arrow"
        variant: "primary"
      - text: "View on GitHub"
        link: "https://github.com/user/repo"
        icon: "external"
        variant: "minimal"

# Sidebar navigation
sidebar:
  # Articles section (from vignettes/)
  articles:
    - label: "Guides"
      autogenerate:
        directory: articles

  # Reference section (auto-generated from Rd files)
  reference:
    - label: "All Functions"
      autogenerate:
        directory: reference

  # Changelog (from NEWS.md)
  news:
    label: "Changelog"
    source: "NEWS.md"

# Features
features:
  katex: true  # LaTeX math rendering (enabled by default)

# Content options
content:
  skip_sections:
    - "author"  # Sections to exclude from function docs

# Output settings
output:
  dir: "docs"
  include_build_files: true
```

## Example Output Embedding

starlightr automatically captures outputs from your `@examples` and
embeds them in the documentation:

- **Plots** - ggplot2 and base R plots are saved as PNGs and displayed
- **Text output** - Console output is captured and shown in a code block
- **HTML widgets** - gt tables, htmlwidgets, etc. are embedded as
  iframes

This happens automatically when you run `build_site()`. Examples are
evaluated in a clean R session and outputs are saved to
`public/examples/`.

To skip example evaluation for specific functions, use `\dontrun{}` in
your Rd files.

## Multi-Version Documentation

starlightr supports deploying multiple versions of your documentation
with a version selector dropdown.

### Configuration

Add a `versions` section to your `_starlightr.yaml`:

``` yaml
versions:
  enabled: true
  current: "1.2.0"  # Current/default version
  list:
    - version: "dev"
      label: "Development"
      path: "/"
    - version: "1.2.0"
      label: "v1.2.0 (latest)"
      path: "/versions/1.2.0/"
    - version: "1.1.0"
      label: "v1.1.0"
      path: "/versions/1.1.0/"
```

### How It Works

1.  Each git tag gets its own documentation build at `/versions/<tag>/`
2.  The version selector dropdown lets users switch between versions
3.  Links use `import.meta.env.BASE_URL` so they work correctly at any
    path

### GitHub Actions Workflow

Generate a deployment workflow that builds and deploys versioned docs:

``` yaml
versions:
  enabled: true
  workflow:
    generate: true
    provider: "cloudflare"  # or "github-pages"
```

Run `build_site()` and the workflow will be created at
`.github/workflows/deploy-docs.yml`.

## Converting from pkgdown

If you already have a `_pkgdown.yml` file, you can convert it:

``` r
pkgdown_to_starlight()
```

This reads your pkgdown configuration and creates a `_starlightr.yaml`
with equivalent settings.

## API Reference

| Function                 | Description                          |
|--------------------------|--------------------------------------|
| `use_starlightr()`       | Initialize starlightr in a package   |
| `build_site()`           | Build the documentation site         |
| `pkgdown_to_starlight()` | Convert pkgdown config to starlightr |

## License

MIT
