# External package link resolution for rendered reference markdown

#' Resolve external package link placeholders in rendered markdown
#'
#' Rust emits external package references as markdown links with a placeholder
#' target. This post-processing step resolves those placeholders to exact URLs
#' via downlit, or degrades unresolved links to plain visible text to match
#' pkgdown's behavior.
#'
#' @param md_content Rendered markdown content
#' @return Markdown content with resolved external package links
#' @keywords internal
resolve_external_package_links <- function(md_content) {
  pattern <- "\\[([^]\\n]+)\\]\\(__STARLIGHTR_EXT_TOPIC__::([^:()\\s]+)::([^()\\s]+)\\)"
  icon_import <- "import { Icon } from '@astrojs/starlight/components';"
  icon_markup <- "<span style = {{ display: 'inline-block', verticalAlign: 'middle' }}><Icon name=\"external\" /></span>"

  matches <- gregexpr(pattern, md_content, perl = TRUE)
  if (matches[[1]][1] == -1) {
    return(md_content)
  }

  match_text <- regmatches(md_content, matches)[[1]]
  captures <- regmatches(match_text, regexec(pattern, match_text, perl = TRUE))
  resolved_any <- FALSE
  replacements <- character(length(captures))

  for (i in seq_along(captures)) {
    parts <- captures[[i]]
    label <- parts[[2]]
    package <- parts[[3]]
    topic <- parts[[4]]

    href <- tryCatch(
      downlit::href_topic(topic, package),
      error = function(e) NA_character_
    )

    replacements[[i]] <- if (is.na(href)) {
      label
    } else {
      resolved_any <- TRUE
      sprintf("[%s](%s)%s", label, href, icon_markup)
    }
  }

  regmatches(md_content, matches) <- list(replacements)

  if (resolved_any && !grepl(icon_import, md_content, fixed = TRUE)) {
    frontmatter_match <- regexpr(
      "^---\\n[\\s\\S]*?\\n---\\n+",
      md_content,
      perl = TRUE
    )

    if (frontmatter_match[1] == 1) {
      frontmatter <- regmatches(md_content, frontmatter_match)
      remainder <- substring(
        md_content,
        frontmatter_match[1] + attr(frontmatter_match, "match.length")
      )
      md_content <- paste0(frontmatter, icon_import, "\n\n", remainder)
    } else {
      md_content <- paste0(icon_import, "\n\n", md_content)
    }
  }

  md_content
}
