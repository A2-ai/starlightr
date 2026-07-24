test_that("include_internal renders internal topics not in sidebar.reference", {
  pkg <- tempfile("pkg")
  dir.create(file.path(pkg, "man"), recursive = TRUE)
  on.exit(unlink(pkg, recursive = TRUE))
  writeLines(
    c("\\name{bsa}", "\\alias{bsa}", "\\title{Calculate Body Surface Area}"),
    file.path(pkg, "man", "bsa.Rd")
  )
  writeLines(
    c(
      "\\name{.bsa_dubois}",
      "\\alias{.bsa_dubois}",
      "\\title{Du Bois body surface area equation}",
      "\\keyword{internal}"
    ),
    file.path(pkg, "man", "dot-bsa_dubois.Rd")
  )
  writeLines(
    c("\\name{crcl}", "\\alias{crcl}", "\\title{Creatinine clearance}"),
    file.path(pkg, "man", "crcl.Rd")
  )

  config <- list(
    sidebar = list(
      reference = list(list(label = "Vitals", contents = list("bsa")))
    )
  )

  # Default: internal topics and unlisted topics are both excluded
  got <- starlightr:::resolve_config_rd_files(pkg, config)
  expect_equal(basename(got), "bsa.Rd")

  # include_internal: internal topics render without a sidebar entry,
  # but unlisted exported topics (crcl) stay excluded
  config$reference <- list(include_internal = TRUE)
  got <- starlightr:::resolve_config_rd_files(pkg, config)
  expect_setequal(basename(got), c("bsa.Rd", "dot-bsa_dubois.Rd"))
})
