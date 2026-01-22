# Markdown post-processing functions for starlightr

#' Fix internal package links in Markdown
#'
#' Transforms links produced by Rd2HTML to work within the Starlight site.
#' Internal links get lowercase slugs without extension for Starlight compatibility.
#'
#' @param md Markdown string
#' @param pkg_name Package name (optional, for context)
#'
#' @return Markdown string with fixed links
#' @keywords internal
fix_internal_links <- function(md, pkg_name = NULL) {
  # Internal help links: [text](../help/topic.html) -> [`text`](../lowercase-topic/)
  # Use ../ to go up from /reference/current/ to /reference/target/
  # Wrap link text in backticks for code formatting
  # Trailing slash required for trailingSlash: 'always' compatibility
  md <- gsub(
    "\\[([^]]+)\\]\\(\\.\\./help/([^)]+)\\.html\\)",
    "[`\\1`](../\\L\\2/)",
    md,
    perl = TRUE
  )

  # Also handle: [text](../../pkgname/help/topic.html) for same package
  if (!is.null(pkg_name)) {
    pattern <- paste0("\\[([^]]+)\\]\\(\\.\\./\\.\\./", pkg_name, "/help/([^)]+)\\.html\\)")
    md <- gsub(pattern, "[`\\1`](../\\L\\2/)", md, perl = TRUE)
  }

  # External package links become plain code text (no link)
  md <- gsub(
    "\\[([^]]+)\\]\\(\\.\\./\\.\\./[^)]+\\)",
    "`\\1`",
    md
  )

  md
}

#' Fix heading levels in Markdown
#'
#' Removes the first h1 (title duplicate). Keeps sections as h2 for Starlight TOC.
#' Rd2HTML produces h3 headings which become h2 after pandoc.
#'
#' @param md Markdown string
#'
#' @return Markdown string with adjusted headings
#' @keywords internal
fix_heading_levels <- function(md) {
  # Remove the first h1 heading (duplicates frontmatter title)
  md <- gsub("^# [^\n]+\n+", "", md)
  md <- gsub("^\n*# [^\n]+\n+", "", md)

  # Convert h3 to h2 (keep sections as h2 for Starlight TOC)
  md <- gsub("^### ", "## ", md)
  md <- gsub("\n### ", "\n## ", md)
  # Note: h2 sections stay as h2 (not promoted to h1) so they appear in TOC

  md
}

#' Convert sourceCode divs to fenced code blocks
#'
#' Rd2HTML outputs code as <div class="sourceCode r">...</div> which pandoc
#' preserves as raw HTML. This converts them to proper markdown code fences.
#'
#' @param md Markdown string
#' @return Markdown string with divs converted to code fences
#' @keywords internal
convert_sourcecode_divs <- function(md) {
  # Match <div class="sourceCode r"> or <div class="sourceCode"> blocks
  # and convert to ```r fenced code blocks
  pattern <- '(?s)<div class="sourceCode[^"]*">\\s*(.*?)\\s*</div>'

  md <- gsub(pattern, "```r\n\\1\n```", md, perl = TRUE)

  md
}

#' Evaluate unevaluated Sexpr expressions in HTML
#'
#' Rd2HTML doesn't evaluate stage=render Sexprs when converting statically.
#' This function finds and evaluates them manually.
#'
#' @param html HTML string potentially containing unevaluated Sexpr
#' @return HTML with Sexpr expressions evaluated
#' @keywords internal
evaluate_sexpr <- function(html) {
  # Pattern matches \Sexpr[...]{code}
  pattern <- "\\\\Sexpr\\[([^\\]]+)\\]\\{([^}]+)\\}"

  # Find all matches
  matches <- gregexpr(pattern, html, perl = TRUE)[[1]]

  if (matches[1] == -1) {
    return(html)
  }

  # Process each match from last to first (to preserve positions)
  match_lengths <- attr(matches, "match.length")

  for (i in rev(seq_along(matches))) {
    start <- matches[i]
    end <- start + match_lengths[i] - 1
    full_match <- substr(html, start, end)

    # Extract the R code
    code_match <- regmatches(full_match, regexec(pattern, full_match, perl = TRUE))[[1]]
    if (length(code_match) < 3) next

    r_code <- code_match[3]

    # Try to evaluate the R expression
    result <- tryCatch(
      {
        eval(parse(text = r_code))
      },
      error = function(e) NULL
    )

    if (!is.null(result)) {
      # Convert \code{} to <code> for HTML
      result <- gsub("\\\\code\\{([^}]+)\\}", "<code>\\1</code>", result)
      # Replace the Sexpr with the result
      html <- paste0(
        substr(html, 1, start - 1),
        result,
        substr(html, end + 1, nchar(html))
      )
    }
  }

  html
}

#' Escape angle brackets outside code blocks for MDX
#'
#' @param md Markdown string
#' @return Markdown with < escaped outside code blocks
#' @keywords internal
escape_angle_brackets <- function(md) {
  # Split by code blocks, escape < only in non-code parts
  parts <- strsplit(md, "(```[^`]*```)", perl = TRUE)[[1]]
  code_blocks <- regmatches(md, gregexpr("```[^`]*```", md, perl = TRUE))[[1]]

  result <- character()
  for (i in seq_along(parts)) {
    part <- parts[i]

    # Convert <URL> autolinks to [URL](URL) format
    part <- gsub("<(https?://[^>]+)>", "[\\1](\\1)", part, perl = TRUE)

    # Escape </ (closing tags) using HTML entity
    part <- gsub("</", "&lt;/", part, fixed = TRUE)

    # Escape < followed by letter or backtick (looks like tag/JSX) using HTML entity
    part <- gsub("<(?=[a-zA-Z`])", "&lt;", part, perl = TRUE)

    # Escape > that follows word chars (closing bracket of tag-like patterns)
    # But not > at start of line (markdown blockquotes)
    part <- gsub("(?<=[a-zA-Z0-9`])>", "&gt;", part, perl = TRUE)

    result <- c(result, part)
    if (i <= length(code_blocks)) {
      result <- c(result, code_blocks[i])
    }
  }

  paste(result, collapse = "")
}

#' Remove unwanted sections from Markdown
#'
#' @param md Markdown string
#' @param skip_sections Character vector of section names to remove
#'
#' @return Markdown string with sections removed
#' @keywords internal
remove_sections <- function(md, skip_sections) {
  if (length(skip_sections) == 0) {
    return(md)
  }

  for (section in skip_sections) {
    # Match ## Section through to next ## or end of string
    # Case insensitive matching for section name
    pattern <- paste0(
      "(?m)^## ", section, "\\s*\n",
      "(?:(?!^## ).)*"
    )
    md <- gsub(pattern, "", md, perl = TRUE, ignore.case = TRUE)
  }

  # Clean up multiple blank lines
  md <- gsub("\n{3,}", "\n\n", md)

  md
}

#' Fix lifecycle badge paths to use CDN
#'
#' Rewrites lifecycle badge image paths from various local/relative formats
#' to the canonical r-lib CDN URL. Handles:
#' - man/figures/lifecycle-*.svg (README badges)
#' - ../help/figures/lifecycle-*.svg (Rd-generated)
#' - ../*/lifecycle-*.svg (other relative paths)
#'
#' @param md Markdown string
#' @return Markdown with lifecycle badges pointing to CDN
#' @keywords internal
fix_lifecycle_badges <- function(md) {
  cdn_base <- "https://lifecycle.r-lib.org/articles/figures/lifecycle-"

  # man/figures/lifecycle-*.svg (common in READMEs)
  md <- gsub(
    "man/figures/lifecycle-([a-z]+)\\.svg",
    paste0(cdn_base, "\\1.svg"),
    md
  )

  # ../help/figures/lifecycle-*.svg and similar relative paths
  # Note: \2 because first group captures optional path segment

  md <- gsub(
    "\\.\\.(/[^)\\s]+)?/lifecycle-([a-z]+)\\.svg",
    paste0(cdn_base, "\\2.svg"),
    md,
    perl = TRUE
  )

  md
}

#' Fix figure paths for Starlight
#'
#' @param md Markdown string
#' @return Markdown with fixed figure paths
#' @keywords internal
fix_figure_paths <- function(md) {
  # R help uses ../help/figures/, we use /figures/
  gsub("\\.\\./help/figures/", "/figures/", md)
}
