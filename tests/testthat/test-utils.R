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

test_that("build_into_staged_dir publishes staged content on success", {
  root <- tempfile("staged-root-")
  dir.create(root, recursive = TRUE)
  final_dir <- file.path(root, "out")

  starlightr:::build_into_staged_dir(final_dir, function(staging_dir) {
    writeLines("fresh", file.path(staging_dir, "a.txt"))
  })

  expect_true(file.exists(file.path(final_dir, "a.txt")))
  expect_equal(readLines(file.path(final_dir, "a.txt")), "fresh")
  # No leftover staging/backup siblings after a successful publish.
  expect_equal(list.files(root), "out")
})

test_that("build_into_staged_dir replaces pre-existing content wholesale on success", {
  root <- tempfile("staged-root-")
  dir.create(root, recursive = TRUE)
  final_dir <- file.path(root, "out")
  dir.create(final_dir)
  writeLines("stale", file.path(final_dir, "stale.txt"))

  starlightr:::build_into_staged_dir(final_dir, function(staging_dir) {
    writeLines("fresh", file.path(staging_dir, "a.txt"))
  })

  expect_true(file.exists(file.path(final_dir, "a.txt")))
  expect_false(file.exists(file.path(final_dir, "stale.txt")))
  expect_equal(list.files(root), "out")
})

test_that("build_into_staged_dir leaves final_dir untouched when build_fn errors", {
  root <- tempfile("staged-root-")
  dir.create(root, recursive = TRUE)
  final_dir <- file.path(root, "out")
  dir.create(final_dir)
  writeLines("original", file.path(final_dir, "keep.txt"))

  expect_error(
    starlightr:::build_into_staged_dir(final_dir, function(staging_dir) {
      writeLines("partial", file.path(staging_dir, "partial.txt"))
      stop("boom")
    }),
    "boom"
  )

  # final_dir is exactly as it was before the call: no partial.txt, and
  # the pre-existing file is untouched.
  expect_equal(list.files(final_dir), "keep.txt")
  expect_equal(readLines(file.path(final_dir, "keep.txt")), "original")
  # No leftover staging/backup siblings after a failed build either.
  expect_equal(list.files(root), "out")
})

test_that("build_into_staged_dir creates final_dir when it doesn't exist yet", {
  root <- tempfile("staged-root-")
  dir.create(root, recursive = TRUE)
  final_dir <- file.path(root, "nested", "out")

  starlightr:::build_into_staged_dir(final_dir, function(staging_dir) {
    writeLines("fresh", file.path(staging_dir, "a.txt"))
  })

  expect_true(file.exists(file.path(final_dir, "a.txt")))
})
