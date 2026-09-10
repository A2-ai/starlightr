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
