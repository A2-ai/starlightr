
<!-- README.md is generated from README.Rmd. Please edit that file -->

# starlightr

<!-- badges: start -->

[![R-CMD-check](https://github.com/A2-ai/starlightr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/A2-ai/starlightr/actions/workflows/R-CMD-check.yaml)
[![extendr](https://img.shields.io/badge/extendr-*-276DC2)](https://extendr.github.io/extendr/extendr_api/)
<!-- badges: end -->

**starlightr** builds Starlight documentation sites for R packages. It
converts Rd files to MDX, turns vignettes into articles, and can embed
captured example outputs directly into reference pages.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("A2-ai/starlightr")
```

## Requirements

- R \>= 4.0
- Node.js \>= 18
- pandoc

## Quick Start

``` r
library(starlightr)

# Run from your package directory
use_starlightr()
build_site(output_dir = "../my-package-docs")
```

Then preview the site:

``` bash
cd ../my-package-docs
bun i
bun run dev
```

Your package should be installed and up to date before running
`build_site()` so starlightr can read Rd documentation and capture
example outputs.

## Reference Files Only

If you already have a Starlight site and only want to regenerate
reference pages from `man/`, use `build_reference_files()`:

``` r
build_reference_files(
  rd_dir = "man",
  output_dir = "../my-package-docs/src/content/docs/reference",
  config_file = "_starlightr.toml",
  site_output_path = "../my-package-docs"
)
```

`site_output_path` should be the root of the Starlight site. It is used
to find captured example outputs in `public/examples/` and append them
to the generated reference pages.

## Configuration

starlightr reads `_starlightr.toml`. A minimal example:

``` toml
[site]
title = "My Package"
description = "A brief description of your package"
logo = "man/figures/logo.png"
favicon = "man/figures/favicon.png"

[home]
hero = { tagline = "What your package does in one sentence", actions = [
    { text = "Get Started", link = "./articles/introduction/", icon = "right-arrow", variant = "primary" },
    { text = "View on GitHub", link = "https://github.com/user/repo", icon = "external", variant = "minimal" }
] }

[sidebar]
articles = [
    { label = "Guides", contents = ["introduction"] }
]
reference = [
    { label = "Reference", autogenerate = { directory = "reference" } }
]
news = { label = "Changelog", source = "NEWS.md" }

[navbar]
right = [
    { icon = "github", href = "https://github.com/user/repo" }
]

[reference]
skip_sections = ["name"]
include_pagefind = true

[output]
dir = "../my-package-docs"
include_build_files = true
```

## Example Outputs

`build_site()` captures outputs from `@examples` and embeds them into
the generated reference pages:

- plots as PNGs
- text output in fenced code blocks
- HTML outputs such as gt tables via iframes

`\dontrun{}` and `\dontshow{}` in Rd are respected and output is not run
or not shown respectively.

## Versions

Versioned docs are supported through the `[versions]` section in
`_starlightr.toml`. When enabled, `build_site()` can also generate a
deployment workflow for multi-version sites.

## pkgdown Migration

If you already have a `_pkgdown.yml`, you can convert it:

``` r
pkgdown_to_starlight()
```

## Main Functions

| Function                  | Purpose                                        |
|---------------------------|------------------------------------------------|
| `use_starlightr()`        | Initialize starlightr in a package             |
| `build_site()`            | Build a full Starlight site                    |
| `build_reference_files()` | Render reference MDX files only                |
| `audit_config()`          | Audit config against package exports and links |
