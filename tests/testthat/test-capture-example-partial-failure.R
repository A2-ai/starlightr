test_that("capture_rd_examples() captures the rest of the batch when one example errors", {
  skip_on_cran()

  pkg_root <- tempfile("testpkg-root-")
  pkg_dir <- file.path(pkg_root, "starlightrtestpkg")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "man"), recursive = TRUE)

  writeLines(
    c(
      "Package: starlightrtestpkg",
      "Version: 0.0.1",
      "Title: Test",
      "Description: Test fixture for capture_rd_examples().",
      "License: MIT"
    ),
    file.path(pkg_dir, "DESCRIPTION")
  )
  writeLines('exportPattern("^[[:alpha:]]+")', file.path(pkg_dir, "NAMESPACE"))
  writeLines(
    c(
      "good_fn <- function() \"ok\"",
      "bad_fn <- function() stop(\"boom\")"
    ),
    file.path(pkg_dir, "R", "fns.R")
  )
  writeLines(
    c(
      "\\name{good_fn}",
      "\\alias{good_fn}",
      "\\title{good_fn}",
      "\\description{good}",
      "\\usage{good_fn()}",
      "\\examples{",
      "good_fn()",
      "}"
    ),
    file.path(pkg_dir, "man", "good_fn.Rd")
  )
  writeLines(
    c(
      "\\name{bad_fn}",
      "\\alias{bad_fn}",
      "\\title{bad_fn}",
      "\\description{bad}",
      "\\usage{bad_fn()}",
      "\\examples{",
      "bad_fn()",
      "}"
    ),
    file.path(pkg_dir, "man", "bad_fn.Rd")
  )

  lib_dir <- tempfile("testpkg-lib-")
  dir.create(lib_dir)
  install.packages(
    pkg_dir,
    repos = NULL,
    type = "source",
    lib = lib_dir,
    quiet = TRUE
  )

  old_libpaths <- .libPaths()
  .libPaths(c(lib_dir, old_libpaths))
  on.exit(
    {
      .libPaths(old_libpaths)
      try(unloadNamespace("starlightrtestpkg"), silent = TRUE)
    },
    add = TRUE
  )

  expect_warning(
    results <- starlightr:::capture_rd_examples(
      "starlightrtestpkg",
      c("good_fn", "bad_fn")
    ),
    regexp = "bad_fn"
  )

  expect_equal(results$good_fn$txt, "[1] \"ok\"")
  expect_null(results$bad_fn)
})
