# Markdown post-processing functions for starlightr

#' Evaluate unevaluated Sexpr expressions in HTML
#'
#' Rd2HTML doesn't evaluate stage=render Sexprs when converting statically.
#' This function finds and evaluates them manually.
#'
#' @param html HTML string potentially containing unevaluated Sexpr
#' @return HTML with Sexpr expressions evaluated
#' @keywords internal
#' @noRd
evaluate_sexpr <- function(html) {
  pattern <- "\\\\Sexpr\\[([^\\]]+)\\]\\{([^}]+)\\}"

  matches <- gregexpr(pattern, html, perl = TRUE)[[1]]
  if (matches[1] == -1) {
    return(html)
  }

  match_lengths <- attr(matches, "match.length")

  for (i in rev(seq_along(matches))) {
    start <- matches[i]
    end <- start + match_lengths[i] - 1
    full_match <- substr(html, start, end)

    code_match <- regmatches(
      full_match,
      regexec(pattern, full_match, perl = TRUE)
    )[[1]]
    if (length(code_match) < 3) {
      next
    }

    r_code <- code_match[3]
    result <- tryCatch(eval(parse(text = r_code)), error = function(e) NULL)

    if (!is.null(result)) {
      result <- gsub("\\\\code\\{([^}]+)\\}", "<code>\\1</code>", result)
      html <- paste0(
        substr(html, 1, start - 1),
        result,
        substr(html, end + 1, nchar(html))
      )
    }
  }

  html
}
