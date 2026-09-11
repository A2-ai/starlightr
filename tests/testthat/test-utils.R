test_that("escape_quoted_string escapes special characters", {
  x <- "C:\\tmp\\file \"name\"\nline2\r\t"
  got <- starlightr:::escape_quoted_string(x)
  expect_equal(got, "C:\\\\tmp\\\\file \\\"name\\\"\\nline2\\r\\t")
})

test_that("fix_img_width renames height on sized-only-by-height img tags", {
  md <- "# `cqtkit` <img src='/figures/readme/logo.png' align=\"right\" height=\"138\" />"
  expect_equal(
    starlightr:::fix_img_width(md),
    "# `cqtkit` <img src='/figures/readme/logo.png' align=\"right\" width=\"138\" />"
  )
})

test_that("fix_img_width handles unquoted, reordered and uppercase attributes", {
  expect_equal(
    starlightr:::fix_img_width("<img src='a.png' height=138 align=right>"),
    "<img src='a.png' width=138 align=right>"
  )
  expect_equal(
    starlightr:::fix_img_width("<img height=\"138\" src='a.png' />"),
    "<img width=\"138\" src='a.png' />"
  )
  expect_equal(
    starlightr:::fix_img_width("<img src='a.png' HEIGHT=\"138\" />"),
    "<img src='a.png' width=\"138\" />"
  )
})

test_that("fix_img_width leaves explicit width and non-tag text alone", {
  both <- "<img src='a.png' width=\"100\" height=\"138\" />"
  expect_equal(starlightr:::fix_img_width(both), both)

  prose <- "plain text with height=\"138\" not in a tag"
  expect_equal(starlightr:::fix_img_width(prose), prose)
})

test_that("rewrite_article_links points cross-vignette links at sibling pages", {
  targets <- c("eda-pk-pkpd-workflow" = "eda-pk-pkpd-workflow")
  md <- "See the [EDA](eda-pk-pkpd-workflow.html) article."
  expect_equal(
    starlightr:::rewrite_article_links(md, targets),
    "See the [EDA](../eda-pk-pkpd-workflow/) article."
  )
})

test_that("rewrite_article_links keeps anchors and slugifies the target", {
  targets <- c("Getting.Started" = "getting-started")
  expect_equal(
    starlightr:::rewrite_article_links("[a](Getting.Started.html#setup)", targets),
    "[a](../getting-started/#setup)"
  )
})

test_that("rewrite_article_links leaves unbuilt and external targets alone", {
  targets <- c("built" = "built")
  md <- paste(
    "[a](other.html)",
    "[b](https://example.org/page.html)",
    "[c](built.html)",
    sep = "\n"
  )
  expect_equal(
    starlightr:::rewrite_article_links(md, targets),
    "[a](other.html)\n[b](https://example.org/page.html)\n[c](../built/)"
  )
})

test_that("is_internal_rd detects the internal keyword", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  plain <- file.path(dir, "plain.Rd")
  internal <- file.path(dir, "internal.Rd")
  writeLines(c("\\name{plain}", "\\title{Plain}"), plain)
  writeLines(c("\\name{hidden}", "\\keyword{internal}"), internal)

  expect_false(starlightr:::is_internal_rd(plain))
  expect_true(starlightr:::is_internal_rd(internal))
})

test_that("classify_unmatched_references separates topics, internals and typos", {
  pkg <- tempfile()
  dir.create(file.path(pkg, "man"), recursive = TRUE)
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  writeLines("\\name{d}", file.path(pkg, "man", "pkg_data.Rd"))
  writeLines("\\name{t}", file.path(pkg, "man", "pkg-compute.Rd"))
  writeLines(
    c("\\name{h}", "\\keyword{internal}"),
    file.path(pkg, "man", "dot-helper.Rd")
  )

  got <- starlightr:::classify_unmatched_references(
    c("pkg_data", "pkg-compute", "dot-helper", "nope"),
    pkg
  )

  expect_equal(sort(got$topics), c("pkg-compute", "pkg_data"))
  expect_equal(got$internal, "dot-helper")
  expect_equal(got$unknown, "nope")
})

test_that("has_documented_topic matches Rd files case-insensitively", {
  pkg <- tempfile()
  dir.create(file.path(pkg, "man"), recursive = TRUE)
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
  writeLines("\\name{x}", file.path(pkg, "man", "pkg-compute.Rd"))

  expect_true(starlightr:::has_documented_topic(pkg, "pkg-compute"))
  expect_true(starlightr:::has_documented_topic(pkg, "PKG-Compute"))
  expect_false(starlightr:::has_documented_topic(pkg, "missing"))
})
