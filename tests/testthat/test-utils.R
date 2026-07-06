test_that("escape_quoted_string escapes special characters", {
  x <- "C:\\tmp\\file \"name\"\nline2\r\t"
  got <- starlightr:::escape_quoted_string(x)
  expect_equal(got, "C:\\\\tmp\\\\file \\\"name\\\"\\nline2\\r\\t")
})

test_that("expand_reference_patterns matches exact names case-insensitively", {
  candidates <- c("build_site", "Build_Articles", "audit_config")
  expect_equal(
    starlightr:::expand_reference_patterns("BUILD_SITE", candidates),
    "build_site"
  )
  expect_equal(
    starlightr:::expand_reference_patterns("build_articles", candidates),
    "Build_Articles"
  )
})

test_that("expand_reference_patterns handles a trailing glob", {
  candidates <- c("build_site", "build_articles", "audit_config")
  expect_equal(
    starlightr:::expand_reference_patterns("build_*", candidates),
    c("build_site", "build_articles")
  )
})

test_that("expand_reference_patterns handles a leading glob", {
  candidates <- c("build_site", "get_site", "audit_config")
  expect_equal(
    starlightr:::expand_reference_patterns("*_site", candidates),
    c("build_site", "get_site")
  )
})

test_that("expand_reference_patterns handles an interior glob", {
  candidates <- c("a_helpers_b", "a_helpers", "other")
  expect_equal(
    starlightr:::expand_reference_patterns("a*b", candidates),
    "a_helpers_b"
  )
})

test_that("expand_reference_patterns matches globs case-insensitively", {
  candidates <- c("Build_Site", "Get_Site", "audit_config")
  expect_equal(
    starlightr:::expand_reference_patterns("build*", candidates),
    "Build_Site"
  )
})

test_that("expand_reference_patterns dedupes across overlapping patterns", {
  candidates <- c("build_site", "build_articles", "audit_config")
  expect_equal(
    starlightr:::expand_reference_patterns(
      c("build_*", "build_site"),
      candidates
    ),
    c("build_site", "build_articles")
  )
})

test_that("expand_reference_patterns returns nothing when there is no match", {
  expect_equal(
    starlightr:::expand_reference_patterns("nope_*", c("build_site")),
    character(0)
  )
})
