test_that("deploy workflows preserve special characters in site paths", {
  root <- tempfile()
  dir.create(root)
  dir.create(file.path(root, ".git"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  paths <- c(".", "nested/docs", "docs & guides", "docs 'quoted'")
  if (.Platform$OS.type != "windows") {
    paths <- c(paths, 'docs "quoted"', "docs\\guides")
  }
  for (path in paths) {
    site <- file.path(root, path)
    dir.create(site, recursive = TRUE, showWarnings = FALSE)
    for (versioned in c(TRUE, FALSE)) {
      starlightr:::generate_deploy_workflow(
        site, list(versions = list(enabled = versioned)), overwrite = TRUE
      )
      workflow <- yaml::read_yaml(
        file.path(root, ".github", "workflows", "deploy-docs.yml")
      )
      if (versioned) {
        expect_identical(workflow$env$DOCS_DIR, path)
      } else {
        steps <- workflow$jobs$deploy$steps
        expect_identical(steps[[3]][["working-directory"]], path)
        expect_identical(steps[[4]]$with$publish_dir, paste0(path, "/dist"))
      }
    }
  }
})

test_that("versioned deployment only redirects to a built default", {
  skip_on_os("windows")
  skip_if(Sys.which("bash") == "", "bash is required")

  workflow <- yaml::yaml.load(starlightr:::render_template(
    "deploy-docs.yml", list(docs_dir = '"."')
  ))
  build <- workflow$jobs$deploy$steps[[3]]$run
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  # Run the actual workflow script with local substitutes for git and Bun.
  # Each successful build produces a page so publication output can be checked.
  run_build <- function(available_tags, default = TRUE) {
    run_dir <- tempfile(tmpdir = root)
    dir.create(run_dir)
    docs <- file.path(run_dir, "docs")
    dir.create(file.path(docs, "src", "data"), recursive = TRUE)
    versions <- starlightr:::render_template("versions.ts", list(
      versions = list(
        list(tag = "2.0.0", label = "Next", default = default),
        list(tag = "1.0.0", label = "Stable", default = FALSE)
      ),
      currentVersion = "dev"
    ))
    writeLines(versions, file.path(docs, "src", "data", "versions.ts"))
    script <- file.path(run_dir, "build.sh")
    writeLines(c(
      paste0("export RUNNER_TEMP=", shQuote(run_dir)),
      paste0("export DOCS_DIR=", shQuote(docs)),
      'export BASE_PREFIX=""',
      paste0("AVAILABLE_TAGS=", shQuote(available_tags)),
      'git() {',
      '  case "$1" in',
      '    fetch|checkout) return 0 ;;',
      '    show)',
      '      case "$2" in',
      '        */versions.ts) cat "$DOCS_DIR/src/data/versions.ts" ;;',
      '        *) return 1 ;;',
      '      esac ;;',
      '    rev-parse)',
      '      case " $AVAILABLE_TAGS " in',
      '        *" ${4#refs/tags/} "*) return 0 ;;',
      '        *) return 1 ;;',
      '      esac ;;',
      '  esac',
      '}',
      'bun() {',
      '  if [ "$1" = run ]; then',
      '    mkdir -p dist',
      '    echo "Built page" > dist/index.html',
      '  fi',
      '}',
      build
    ), script)
    output <- suppressWarnings(system2(
      "bash", c("-e", shQuote(script)), stdout = TRUE, stderr = TRUE
    ))
    list(
      status = attr(output, "status") %||% 0L,
      output = paste(output, collapse = "\n"),
      redirect = file.path(run_dir, "site", "index.html")
    )
  }

  missing_default <- run_build("v1.0.0")
  expect_equal(missing_default$status, 1L)
  expect_match(missing_default$output, "Default version 2.0.0 was not built")
  expect_false(file.exists(missing_default$redirect))

  none_built <- run_build("")
  expect_equal(none_built$status, 1L)
  expect_match(none_built$output, "No documentation versions were built")
  expect_false(file.exists(none_built$redirect))

  for (default in c(TRUE, FALSE)) {
    success <- run_build("v2.0.0", default = default)
    expect_equal(success$status, 0L)
    expect_match(
      paste(readLines(success$redirect), collapse = "\n"),
      "url=/versions/2.0.0/", fixed = TRUE, useBytes = TRUE
    )
  }
})
