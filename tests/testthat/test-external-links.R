test_that("build_external_link_map resolves known dplyr topics", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rlang")

  json_path <- starlightr:::build_external_link_map(
    system.file(package = "dplyr")
  )
  on.exit(unlink(json_path))

  map <- jsonlite::fromJSON(json_path)

  # rlang is a dependency of dplyr, so its aliases should be in the map
  expect_equal(
    map[["rlang::sym"]],
    "https://rlang.r-lib.org/reference/sym.html"
  )
})

test_that("render_reference resolves external link with provided link map", {
  skip_if_not_installed("dplyr")

  fixture <- testthat::test_path("../testdata/hyperion-tables-section-rules.Rd")

  out_dir <- tempfile("starlightr-ref-")
  dir.create(out_dir, recursive = TRUE)

  config_path <- file.path(tempdir(), "starlightr-test.toml")
  writeLines("[reference]\ninclude_pagefind = false\n", config_path)

  links_path <- starlightr:::build_external_link_map(
    system.file(package = "dplyr")
  )

  starlightr::render_reference(fixture, out_dir, config_path, links_path)

  out_file <- file.path(out_dir, "hyperion-tables-section-rules.mdx")
  expect_snapshot_file(out_file, "external-links.mdx")
})
