# Regression tests for F7: capture_rd_examples() used to (a) hard-require
# ggplot2/gt even for packages whose examples never use them, (b) overwrite
# png_raw/html on every iteration so only the last plot/table of a
# multi-output example survived, and (c) leave the target package attached to
# the caller's session forever. See .review/findings-2026-07-06.md.

make_capture_fixture_pkg <- function(pkg_name, r_lines, rd_files) {
  pkg_root <- tempfile("testpkg-root-")
  pkg_dir <- file.path(pkg_root, pkg_name)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "man"), recursive = TRUE)

  writeLines(
    c(
      paste0("Package: ", pkg_name),
      "Version: 0.0.1",
      "Title: Test",
      "Description: Test fixture for capture_rd_examples().",
      "License: MIT"
    ),
    file.path(pkg_dir, "DESCRIPTION")
  )
  writeLines('exportPattern("^[[:alpha:]]+")', file.path(pkg_dir, "NAMESPACE"))
  writeLines(r_lines, file.path(pkg_dir, "R", "fns.R"))

  for (rd_name in names(rd_files)) {
    writeLines(rd_files[[rd_name]], file.path(pkg_dir, "man", rd_name))
  }

  pkg_dir
}

install_fixture_pkg <- function(pkg_dir) {
  lib_dir <- tempfile("testpkg-lib-")
  dir.create(lib_dir)
  install.packages(
    pkg_dir,
    repos = NULL,
    type = "source",
    lib = lib_dir,
    quiet = TRUE
  )
  lib_dir
}

test_that("capture_rd_examples() keeps every plot/table an example produces, not just the last", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gt")

  pkg_name <- "starlightrmultipkg"
  pkg_dir <- make_capture_fixture_pkg(
    pkg_name,
    # capture_rd_examples() only captures a plot/table when the top-level
    # example expression is itself visibly a ggplot/gt_tbl object -- so the
    # two plots/tables must be separate top-level statements, not values
    # printed from inside a helper function (whose own return would hide
    # them from capture's visibility check).
    r_lines = c("multi_plot_fn <- function() invisible(NULL)", "multi_table_fn <- function() invisible(NULL)"),
    rd_files = list(
      "multi_plot_fn.Rd" = c(
        "\\name{multi_plot_fn}",
        "\\alias{multi_plot_fn}",
        "\\title{multi_plot_fn}",
        "\\description{multi_plot_fn}",
        "\\usage{multi_plot_fn()}",
        "\\examples{",
        "ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()",
        "ggplot2::ggplot(data.frame(x = 2, y = 2), ggplot2::aes(x, y)) + ggplot2::geom_point()",
        "}"
      ),
      "multi_table_fn.Rd" = c(
        "\\name{multi_table_fn}",
        "\\alias{multi_table_fn}",
        "\\title{multi_table_fn}",
        "\\description{multi_table_fn}",
        "\\usage{multi_table_fn()}",
        "\\examples{",
        "gt::gt(data.frame(a = 1))",
        "gt::gt(data.frame(a = 2))",
        "}"
      )
    )
  )

  lib_dir <- install_fixture_pkg(pkg_dir)
  old_libpaths <- .libPaths()
  .libPaths(c(lib_dir, old_libpaths))
  on.exit(
    {
      .libPaths(old_libpaths)
      try(unloadNamespace(pkg_name), silent = TRUE)
    },
    add = TRUE
  )

  results <- starlightr:::capture_rd_examples(
    pkg_name,
    c("multi_plot_fn", "multi_table_fn")
  )

  expect_length(results$multi_plot_fn$png_raw, 2)
  expect_true(all(vapply(results$multi_plot_fn$png_raw, is.raw, logical(1))))

  expect_length(results$multi_table_fn$html, 2)
  expect_true(all(vapply(results$multi_table_fn$html, is.character, logical(1))))
})

test_that("capture_rd_examples() detaches the target package if it attached it", {
  skip_on_cran()

  pkg_name <- "starlightrdetachpkg"
  pkg_dir <- make_capture_fixture_pkg(
    pkg_name,
    r_lines = c("plain_fn <- function() \"ok\""),
    rd_files = list(
      "plain_fn.Rd" = c(
        "\\name{plain_fn}",
        "\\alias{plain_fn}",
        "\\title{plain_fn}",
        "\\description{plain_fn}",
        "\\usage{plain_fn()}",
        "\\examples{",
        "plain_fn()",
        "}"
      )
    )
  )

  lib_dir <- install_fixture_pkg(pkg_dir)
  old_libpaths <- .libPaths()
  .libPaths(c(lib_dir, old_libpaths))
  on.exit(
    {
      .libPaths(old_libpaths)
      try(unloadNamespace(pkg_name), silent = TRUE)
    },
    add = TRUE
  )

  expect_false(paste0("package:", pkg_name) %in% search())

  results <- starlightr:::capture_rd_examples(pkg_name, "plain_fn")

  expect_equal(results$plain_fn$txt, "[1] \"ok\"")
  expect_false(
    paste0("package:", pkg_name) %in% search(),
    label = "target package should not remain attached after capture_rd_examples() returns"
  )
})

test_that("capture_rd_examples() does not require ggplot2/gt when no example uses them", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (package %in% c("ggplot2", "gt")) FALSE else base::requireNamespace(package, ...)
    }
  )

  pkg_name <- "starlightrdetachpkg2"
  pkg_dir <- make_capture_fixture_pkg(
    pkg_name,
    r_lines = c("plain_fn2 <- function() \"ok\""),
    rd_files = list(
      "plain_fn2.Rd" = c(
        "\\name{plain_fn2}",
        "\\alias{plain_fn2}",
        "\\title{plain_fn2}",
        "\\description{plain_fn2}",
        "\\usage{plain_fn2()}",
        "\\examples{",
        "plain_fn2()",
        "}"
      )
    )
  )

  lib_dir <- install_fixture_pkg(pkg_dir)
  old_libpaths <- .libPaths()
  .libPaths(c(lib_dir, old_libpaths))
  on.exit(
    {
      .libPaths(old_libpaths)
      try(unloadNamespace(pkg_name), silent = TRUE)
    },
    add = TRUE
  )

  results <- starlightr:::capture_rd_examples(pkg_name, "plain_fn2")

  expect_equal(results$plain_fn2$txt, "[1] \"ok\"")
})

test_that("capture_rd_examples() warns and skips (not hard-stops) when a needed plotting package is missing", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (package == "ggplot2") FALSE else base::requireNamespace(package, ...)
    }
  )

  pkg_name <- "starlightrmissingggplot"
  pkg_dir <- make_capture_fixture_pkg(
    pkg_name,
    r_lines = c("plot_and_text_fn <- function() invisible(NULL)"),
    rd_files = list(
      "plot_and_text_fn.Rd" = c(
        "\\name{plot_and_text_fn}",
        "\\alias{plot_and_text_fn}",
        "\\title{plot_and_text_fn}",
        "\\description{plot_and_text_fn}",
        "\\usage{plot_and_text_fn()}",
        "\\examples{",
        "ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()",
        "\"ok\"",
        "}"
      )
    )
  )

  lib_dir <- install_fixture_pkg(pkg_dir)
  old_libpaths <- .libPaths()
  .libPaths(c(lib_dir, old_libpaths))
  on.exit(
    {
      .libPaths(old_libpaths)
      try(unloadNamespace(pkg_name), silent = TRUE)
    },
    add = TRUE
  )

  expect_warning(
    results <- starlightr:::capture_rd_examples(pkg_name, "plot_and_text_fn"),
    regexp = "ggplot2"
  )

  expect_null(results$plot_and_text_fn$png_raw)
  expect_equal(results$plot_and_text_fn$txt, "[1] \"ok\"")
})
