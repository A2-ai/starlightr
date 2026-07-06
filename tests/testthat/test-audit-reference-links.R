# Regression tests for F8: audit_config() validated `./reference/<x>/` links
# against NAMESPACE exports only, so `@rdname`-grouped topics and
# `<pkg>-package` doc pages (both valid reference targets, neither an export)
# produced false "Reference link target not found" warnings.

test_that("validate_link_target accepts a -package doc link that isn't an export", {
  pkg <- tempfile("pkg-")
  dir.create(pkg)
  reference_slugs <- c("demo-package", "foo")

  expect_false(starlightr:::validate_link_target(
    "./reference/demo-package/", "test", pkg, reference_slugs
  ))
})

test_that("validate_link_target accepts an @rdname topic that isn't itself an export", {
  pkg <- tempfile("pkg-")
  dir.create(pkg)
  # `bar` is the Rd topic several functions are grouped under via @rdname;
  # the topic name need not match any individual export.
  reference_slugs <- c("bar", "foo")

  expect_false(starlightr:::validate_link_target(
    "./reference/bar/", "test", pkg, reference_slugs
  ))
})

test_that("validate_link_target still flags links to nonexistent reference pages", {
  pkg <- tempfile("pkg-")
  dir.create(pkg)
  reference_slugs <- c("foo")

  expect_warning(
    result <- starlightr:::validate_link_target(
      "./reference/nope/", "test", pkg, reference_slugs
    ),
    "Reference link target not found"
  )
  expect_true(result)
})

test_that("audit_config() does not false-warn on -package and @rdname reference links", {
  pkg <- tempfile("pkg-")
  man_dir <- file.path(pkg, "man")
  dir.create(man_dir, recursive = TRUE)

  writeLines(
    c("Package: demo", "Version: 0.0.1", "Title: Demo"),
    file.path(pkg, "DESCRIPTION")
  )

  # `foo` is an ordinary exported function with a matching Rd topic.
  writeLines(
    c("\\name{foo}", "\\alias{foo}", "\\title{Foo}", "\\description{Foo.}"),
    file.path(man_dir, "foo.Rd")
  )

  # `bar` is an @rdname topic grouping two exports, neither named `bar`.
  writeLines(
    c(
      "\\name{bar}",
      "\\alias{baz}",
      "\\alias{qux}",
      "\\title{Bar}",
      "\\description{Bar.}"
    ),
    file.path(man_dir, "bar.Rd")
  )

  # `demo-package` is the package doc page: a valid reference target that is
  # never a NAMESPACE export.
  writeLines(
    c(
      "\\docType{package}",
      "\\name{demo-package}",
      "\\alias{demo-package}",
      "\\title{demo: A Demo Package}",
      "\\description{A demo package used only for testing.}"
    ),
    file.path(man_dir, "demo-package.Rd")
  )

  writeLines(
    c(
      "export(foo)",
      "export(baz)",
      "export(qux)"
    ),
    file.path(pkg, "NAMESPACE")
  )

  writeLines(
    c(
      "[[sidebar.top]]",
      'link = "./reference/demo-package/"',
      "",
      "[[sidebar.top2]]",
      'link = "./reference/bar/"',
      "",
      "[[sidebar.top3]]",
      'link = "./reference/nope/"'
    ),
    file.path(pkg, "_starlightr.toml")
  )

  expect_warning(
    starlightr::audit_config(pkg),
    "Reference link target not found.*nope"
  )

  warnings <- testthat::capture_warnings(starlightr::audit_config(pkg))
  expect_false(any(grepl("demo-package", warnings)))
  expect_false(any(grepl("\\bbar\\b", warnings)))
  expect_true(any(grepl("nope", warnings)))
})
