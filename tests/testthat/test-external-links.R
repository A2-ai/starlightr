test_that("resolve_external_package_links degrades unresolved links to plain text", {
  input <- "[`foo::does_not_exist()`](__STARLIGHTR_EXT_TOPIC__::foo::does_not_exist)"

  expect_equal(
    starlightr:::resolve_external_package_links(input),
    "`foo::does_not_exist()`"
  )
})

test_that("render_reference resolves external package links end-to-end", {
  skip_if_not_installed("dplyr")

  fixture <- testthat::test_path(
    "../../src/rust/parser/test_data/hyperion-tables-section-rules.Rd"
  )

  out_dir <- tempfile("starlightr-ref-")
  dir.create(out_dir, recursive = TRUE)

  config_path <- file.path(tempdir(), "starlightr-test.toml")
  writeLines("[reference]\ninclude_pagefind = false\n", config_path)

  starlightr::render_reference(fixture, out_dir, config_path)

  out_file <- file.path(out_dir, "hyperion-tables-section-rules.mdx")
  md_content <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
  md_content <- starlightr:::resolve_external_package_links(md_content)
  writeLines(md_content, out_file)

  expect_snapshot_file(out_file, "external-links.mdx")
})
