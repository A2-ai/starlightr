make_reference_dir_fixture <- function() {
  pkg <- tempfile("pkg-")
  man_dir <- file.path(pkg, "man")
  dir.create(man_dir, recursive = TRUE)

  writeLines(
    c("Package: demo", "Version: 0.0.1", "Title: Demo", "Imports: cli"),
    file.path(pkg, "DESCRIPTION")
  )

  writeLines(
    c(
      "\\name{public_fn}",
      "\\alias{public_fn}",
      "\\title{public_fn}",
      "\\description{",
      "A public function.",
      "}"
    ),
    file.path(man_dir, "public_fn.Rd")
  )

  writeLines(
    c(
      "\\name{internal_fn}",
      "\\alias{internal_fn}",
      "\\title{internal_fn}",
      "\\description{",
      "An internal helper.",
      "}",
      "\\keyword{internal}"
    ),
    file.path(man_dir, "internal_fn.Rd")
  )

  pkg
}

test_that("build_reference_dir() filters out @keywords internal by default", {
  pkg <- make_reference_dir_fixture()
  out_dir <- file.path(pkg, "out")

  written <- build_reference_dir(
    rd_dir = "man",
    output_dir = out_dir,
    pkg = pkg,
    examples = FALSE
  )

  expect_equal(written, file.path(out_dir, "public_fn.mdx"))
  expect_true(file.exists(file.path(out_dir, "public_fn.mdx")))
  expect_false(file.exists(file.path(out_dir, "internal_fn.mdx")))
})

test_that("build_reference_dir() includes internal functions when include_internal = TRUE", {
  pkg <- make_reference_dir_fixture()
  out_dir <- file.path(pkg, "out")

  written <- build_reference_dir(
    rd_dir = "man",
    output_dir = out_dir,
    pkg = pkg,
    examples = FALSE,
    include_internal = TRUE
  )

  expect_setequal(
    written,
    file.path(out_dir, c("public_fn.mdx", "internal_fn.mdx"))
  )
  expect_true(file.exists(file.path(out_dir, "internal_fn.mdx")))
})

test_that("build_reference_dir() respects reference.include_internal from config", {
  pkg <- make_reference_dir_fixture()
  out_dir <- file.path(pkg, "out")

  writeLines(
    c("[reference]", "include_internal = true"),
    file.path(pkg, "_starlightr.toml")
  )

  written <- build_reference_dir(
    rd_dir = "man",
    output_dir = out_dir,
    pkg = pkg,
    examples = FALSE
  )

  expect_true(file.exists(file.path(out_dir, "internal_fn.mdx")))
})

test_that("build_reference_dir() warns and returns character(0) when nothing survives filtering", {
  pkg <- tempfile("pkg-")
  man_dir <- file.path(pkg, "man")
  dir.create(man_dir, recursive = TRUE)
  writeLines(c("Package: demo", "Version: 0.0.1", "Title: Demo"), file.path(pkg, "DESCRIPTION"))

  out_dir <- file.path(pkg, "out")

  expect_warning(
    written <- build_reference_dir(rd_dir = "man", output_dir = out_dir, pkg = pkg),
    regexp = "No Rd files"
  )
  expect_equal(written, character())
})
