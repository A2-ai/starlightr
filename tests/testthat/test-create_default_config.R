test_that("create_default_config writes valid TOML for escaped Description", {
  pkg <- tempfile("pkg-")
  dir.create(pkg, recursive = TRUE)

  desc <- 'Windows path C:\\q\\docs with "quotes" and tab\tend'

  writeLines(
    c(
      "Package: demo",
      "Version: 0.0.1",
      "Title: Demo",
      paste0("Description: ", desc)
    ),
    file.path(pkg, "DESCRIPTION")
  )

  cfg <- file.path(pkg, "_starlightr.toml")
  starlightr:::create_default_config(cfg, pkg)

  parsed <- tomledit::from_toml(tomledit::read_toml(cfg))

  expect_equal(parsed$site$title, "demo")
  expect_equal(parsed$site$description, desc)
})
