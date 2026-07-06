# Regression tests for F5: a single escaping boundary for user text
# interpolated into YAML frontmatter, MDX, and JS/TS templates.

test_that("escape_mdx_attr escapes quotes and ampersands for JSX attribute values", {
  expect_equal(
    starlightr:::escape_mdx_attr('Tom & "Jerry"'),
    "Tom &amp; &quot;Jerry&quot;"
  )
})

test_that("escape_mdx_text escapes a leading/embedded angle bracket", {
  expect_equal(
    starlightr:::escape_mdx_text("a <b> c"),
    "a \\<b> c"
  )
})

test_that("process_news escapes a quoted NEWS label into valid YAML frontmatter", {
  pkg <- tempfile("dir-")
  dir.create(pkg, recursive = TRUE)
  writeLines(c("# demo 0.0.1", "", "* Initial release."), file.path(pkg, "NEWS.md"))

  output_path <- tempfile("dir-")
  docs_dir <- file.path(output_path, "src", "content", "docs")
  dir.create(docs_dir, recursive = TRUE)

  config <- list(sidebar = list(news = list(label = 'The "Changelog"')))
  starlightr:::process_news(pkg, output_path, config)

  news_lines <- readLines(file.path(docs_dir, "news.mdx"))
  fm <- paste(news_lines[seq(2, which(news_lines == "---")[2] - 1)], collapse = "\n")
  expect_equal(yaml::yaml.load(fm)$title, 'The "Changelog"')
})

test_that("process_article_inline escapes a quoted title into valid YAML frontmatter", {
  source_dir <- tempfile("dir-")
  dir.create(source_dir, recursive = TRUE)
  writeLines("Some article body.", file.path(source_dir, "article.md"))

  output_dir <- tempfile("dir-")
  dir.create(output_dir, recursive = TRUE)
  figures_dir <- tempfile("dir-")
  dir.create(figures_dir, recursive = TRUE)

  out_file <- starlightr:::process_article_inline(
    slug = "article",
    md_name = "article",
    source_dir = source_dir,
    output_dir = output_dir,
    title = 'A "Quoted" Title',
    public_figures_dir = figures_dir
  )

  lines <- readLines(out_file)
  fm <- paste(lines[seq(2, which(lines == "---")[2] - 1)], collapse = "\n")
  expect_equal(yaml::yaml.load(fm)$title, 'A "Quoted" Title')
})

test_that("create_index_page escapes quotes/ampersands/angle-brackets in title, hero, and cards", {
  pkg <- tempfile("dir-")
  dir.create(pkg, recursive = TRUE)
  writeLines(
    c("Package: demo", "Version: 0.0.1", "Title: Demo"),
    file.path(pkg, "DESCRIPTION")
  )

  output_path <- tempfile("dir-")
  dir.create(
    file.path(output_path, "src", "content", "docs"),
    recursive = TRUE
  )

  config <- list(
    site = list(title = 'Demo "Site"', description = "A & B"),
    home = list(
      hero = list(
        tagline = 'Build "docs" fast',
        actions = list(list(text = 'Go "Now"', link = "/x?a=1&b=2"))
      ),
      cards = list(
        list(
          title = 'Card "One"',
          description = "See <this>",
          link = "/one/",
          icon = 'star"'
        )
      )
    )
  )

  starlightr:::create_index_page(pkg, output_path, config)

  index_path <- file.path(output_path, "src", "content", "docs", "index.mdx")
  content <- paste(readLines(index_path), collapse = "\n")

  # Frontmatter (title/description/tagline) must remain parseable YAML.
  lines <- readLines(index_path)
  fence_lines <- which(lines == "---")
  fm <- paste(lines[seq(fence_lines[1] + 1, fence_lines[2] - 1)], collapse = "\n")
  fm_parsed <- yaml::yaml.load(fm)
  expect_equal(fm_parsed$title, 'Demo "Site"')
  expect_equal(fm_parsed$hero$tagline, 'Build "docs" fast')

  # Hero action text is inside a quoted YAML scalar too.
  expect_match(content, 'text: "Go \\"Now\\""', fixed = TRUE)

  # Card title/icon sit inside a double-quoted JSX attribute: no raw `"`.
  expect_match(content, '<Card title="Card &quot;One&quot;" icon="star&quot;">', fixed = TRUE)
  # Card description sits in MDX body text: no raw `<`.
  expect_match(content, "See \\<this>", fixed = TRUE)
})

test_that("make_sidebar_item escapes backslashes as well as quotes", {
  item <- starlightr:::make_sidebar_item('back\\slash "quote"', "slug")
  expect_equal(item, '{ label: "back\\\\slash \\"quote\\"", slug: "slug" }')
})

test_that("make_sidebar_group escapes control characters like make_sidebar_item", {
  group <- starlightr:::make_sidebar_group("Tab\tGroup", c("item1"))
  expect_match(group, "Tab\\tGroup", fixed = TRUE)
})

test_that("generate_astro_config escapes a quoted github URL for the JS string literal", {
  output_path <- tempfile("dir-")
  dir.create(output_path, recursive = TRUE)

  config <- list(
    site = list(title = "Demo"),
    navbar = list(right = list(list(icon = "github", href = 'https://x/"y"')))
  )

  starlightr:::generate_astro_config(output_path, config)

  content <- paste(
    readLines(file.path(output_path, "astro.config.mjs")),
    collapse = "\n"
  )
  expect_match(
    content,
    'href: "https://x/\\"y\\""',
    fixed = TRUE
  )
})

test_that("generate_versions_ts escapes tags/labels as JS strings, not HTML entities", {
  output_path <- tempfile("dir-")

  config <- list(versions = list(list = list(
    list(tag = 'v"1"', label = 'v"1"', default = TRUE)
  )))

  starlightr:::generate_versions_ts(output_path, config)

  content <- paste(
    readLines(file.path(output_path, "src", "data", "versions.ts")),
    collapse = "\n"
  )
  # Must be a valid JS string escape, not whisker's default HTML entity.
  expect_match(content, 'tag: "v\\"1\\""', fixed = TRUE)
  expect_false(grepl("&quot;", content, fixed = TRUE))
})
