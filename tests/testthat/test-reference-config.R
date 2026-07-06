test_that("write_reference_config_toml round-trips vectors of any length", {
  # tomledit::as_toml() collapses length-1 character vectors to bare TOML
  # strings, which the Rust side (expects Vec<String>) fails to deserialize.
  # write_reference_config_toml() must always emit arrays.
  path <- starlightr:::write_reference_config_toml(list(
    skip_sections = "name",
    section_order = c("Title", "Description"),
    include_pagefind = TRUE
  ))

  parsed <- tomledit::from_toml(tomledit::read_toml(path))

  # tomledit::from_toml() itself returns single-element TOML arrays as a
  # list rather than an atomic vector; unlist() normalizes for comparison.
  expect_equal(unlist(parsed$reference$skip_sections), "name")
  expect_equal(unlist(parsed$reference$section_order), c("Title", "Description"))
  expect_true(parsed$reference$include_pagefind)
})

test_that("write_reference_config_toml escapes quotes/backslashes in section names", {
  path <- starlightr:::write_reference_config_toml(list(
    skip_sections = c('weird "name"', "back\\slash"),
    section_order = character(),
    include_pagefind = FALSE
  ))

  parsed <- tomledit::from_toml(tomledit::read_toml(path))

  expect_equal(parsed$reference$skip_sections, c('weird "name"', "back\\slash"))
})

test_that("default reference skip_sections spelling matches the emitted section key", {
  # `\docType` lowers to a section titled "Doc Type" (sections.rs); the skip
  # token has to match that (case-insensitively) or the default is a no-op.
  cfg <- starlightr:::default_config()
  expect_true(any(tolower(cfg$reference$skip_sections) == "doc type"))
})

test_that("build_reference_files() applies default reference config end-to-end", {
  pkg <- tempfile("pkg-")
  man_dir <- file.path(pkg, "man")
  dir.create(man_dir, recursive = TRUE)

  # A real dependency (installed, on this machine) so
  # extract_dependency_packages()/build_external_link_map() has something to
  # resolve; unrelated to this fix, just needed to exercise the real
  # build_reference_files() entry point end-to-end.
  writeLines(
    c("Package: demo", "Version: 0.0.1", "Title: Demo", "Imports: cli"),
    file.path(pkg, "DESCRIPTION")
  )

  rd <- c(
    "\\docType{package}",
    "\\name{demo-package}",
    "\\alias{demo}",
    "\\alias{demo-package}",
    "\\title{demo: A Demo Package}",
    "\\description{",
    "A demo package used only for testing.",
    "}"
  )
  writeLines(rd, file.path(man_dir, "demo-package.Rd"))

  out_dir <- file.path(pkg, "out")

  # No _starlightr.toml in `pkg` -> read_config() falls back to
  # default_config(), which is the scenario F1 covers: the scaffolded
  # config has no [reference] table.
  written <- build_reference_files(
    rd_files = file.path(man_dir, "demo-package.Rd"),
    output_dir = out_dir,
    pkg = pkg,
    config_file = "_starlightr.toml",
    examples = FALSE
  )

  mdx <- readLines(written)

  expect_false(any(grepl("^## Doc Type$", mdx)))
  expect_false(any(grepl("^## Name$", mdx)))
  expect_false(any(grepl("^## Alias$", mdx)))
  expect_false(any(grepl("^## Title$", mdx)))
  expect_true(any(grepl("^## Description$", mdx)))
})
