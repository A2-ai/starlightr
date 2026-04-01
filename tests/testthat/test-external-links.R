test_that("build_external_link_map resolves known dplyr topics", {
  skip_if_not_installed("dplyr")

  json_path <- starlightr:::build_external_link_map(c("dplyr"))
  on.exit(unlink(json_path))

  map <- jsonlite::fromJSON(json_path)

  expect_equal(
    map[["dplyr::case_when"]],
    "https://dplyr.tidyverse.org/reference/case_when.html"
  )
})
