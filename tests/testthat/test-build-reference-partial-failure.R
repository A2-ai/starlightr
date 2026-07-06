test_that("build_reference_files() renders the rest of the batch when one Rd file fails to parse", {
  pkg <- tempfile("pkg-")
  man_dir <- file.path(pkg, "man")
  dir.create(man_dir, recursive = TRUE)

  writeLines(
    c("Package: demo", "Version: 0.0.1", "Title: Demo", "Imports: cli"),
    file.path(pkg, "DESCRIPTION")
  )

  good_rd <- c(
    "\\name{good_fn}",
    "\\alias{good_fn}",
    "\\title{good_fn}",
    "\\description{",
    "A function that renders fine.",
    "}"
  )
  writeLines(good_rd, file.path(man_dir, "good_fn.Rd"))

  # Unbalanced brace: the Rust parser hits EOF mid-document and returns an
  # error (previously this aborted the whole render_reference() call chain
  # and, with it, the rest of the batch).
  bad_rd <- c("\\name{bad_fn", "\\alias{bad_fn}")
  writeLines(bad_rd, file.path(man_dir, "bad_fn.Rd"))

  out_dir <- file.path(pkg, "out")

  expect_warning(
    written <- build_reference_files(
      rd_files = c(
        file.path(man_dir, "good_fn.Rd"),
        file.path(man_dir, "bad_fn.Rd")
      ),
      output_dir = out_dir,
      pkg = pkg,
      config_file = "_starlightr.toml",
      examples = FALSE
    ),
    regexp = "bad_fn"
  )

  expect_equal(written, file.path(out_dir, "good_fn.mdx"))
  expect_true(file.exists(file.path(out_dir, "good_fn.mdx")))
  expect_false(file.exists(file.path(out_dir, "bad-fn.mdx")))
})
