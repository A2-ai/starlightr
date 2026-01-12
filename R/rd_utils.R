# Core Rd parsing utilities for starlightr

#' Check if pandoc is available
#'
#' @return Logical, TRUE if pandoc is available
#' @keywords internal
check_pandoc <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    return(FALSE)
  }
  rmarkdown::pandoc_available()
}

#' Ensure pandoc is available, error if not
#'
#' @return NULL (called for side effect)
#' @keywords internal
require_pandoc <- function() {
  if (!check_pandoc()) {
    cli::cli_abort(c(
      "Pandoc is required but not found.",
      "i" = "Install pandoc: {.url https://pandoc.org/installing.html}",
      "i" = "Or install the {.pkg rmarkdown} package which includes pandoc."
    ))
  }
  invisible(NULL)
}

#' Convert HTML string to Markdown using pandoc
#'
#' @param html Character string containing HTML
#' @param wrap Logical, whether to wrap long lines (default FALSE)
#'
#' @return Character string containing Markdown
#' @keywords internal
html_to_md <- function(html, wrap = FALSE) {
  require_pandoc()

  if (is.null(html) || length(html) == 0 || nchar(trimws(html)) == 0) {
    return("")
  }

  wrap_arg <- if (wrap) "--wrap=auto" else "--wrap=none"

  result <- tryCatch(
    {
      system2(
        rmarkdown::pandoc_exec(),
        args = c("-f", "html", "-t", "gfm", wrap_arg),
        input = html,
        stdout = TRUE,
        stderr = FALSE
      )
    },
    error = function(e) {
      cli::cli_warn("Pandoc conversion failed: {e$message}")
      # Fallback: strip HTML tags
      gsub("<[^>]+>", "", html)
    }
  )

  paste(result, collapse = "\n")
}

#' Get a specific section from an Rd object
#'
#' @param rd_obj Rd object from tools::Rd_db
#' @param tag Section tag (e.g., "description", "arguments")
#'
#' @return The Rd element for that section, or NULL if not found
#' @keywords internal
get_rd_section <- function(rd_obj, tag) {
  if (!startsWith(tag, "\\")) {
    tag <- paste0("\\", tag)
  }

  for (element in rd_obj) {
    element_tag <- attr(element, "Rd_tag")
    if (!is.null(element_tag) && element_tag == tag) {
      return(element)
    }
  }
  NULL
}

#' Get all aliases from an Rd object
#'
#' @param rd_obj Rd object from tools::Rd_db
#'
#' @return Character vector of aliases
#' @keywords internal
get_rd_aliases <- function(rd_obj) {
  aliases <- character()
  for (element in rd_obj) {
    element_tag <- attr(element, "Rd_tag")
    if (!is.null(element_tag) && element_tag == "\\alias") {
      alias <- trimws(paste(unlist(element), collapse = ""))
      aliases <- c(aliases, alias)
    }
  }
  aliases
}

#' Extract plain text from an Rd section (for frontmatter)
#'
#' @param rd_obj Rd object
#' @param section Section name
#'
#' @return Plain text string or NULL
#' @keywords internal
extract_rd_text <- function(rd_obj, section) {
  element <- get_rd_section(rd_obj, section)
  if (is.null(element)) {
    return(NULL)
  }

  # Flatten to text, removing Rd markup
  text <- paste(unlist(element), collapse = " ")
  text <- trimws(gsub("\\s+", " ", text))

  if (nchar(text) == 0) NULL else text
}

#' Check if an Rd section contains a specific tag
#'
#' Recursively checks if an Rd section contains elements with specified tags.
#'
#' @param rd_section An Rd section
#' @param tags Character vector of tags to look for (e.g., c("\\eqn", "\\deqn"))
#' @return Logical indicating if any of the tags are present
#' @keywords internal
section_has_tag <- function(rd_section, tags) {
  if (is.null(rd_section)) return(FALSE)

  check_element <- function(el) {
    if (is.character(el)) return(FALSE)

    tag <- attr(el, "Rd_tag")
    if (!is.null(tag) && tag %in% tags) {
      return(TRUE)
    }

    if (is.list(el)) {
      return(any(vapply(el, check_element, logical(1))))
    }

    FALSE
  }

  check_element(rd_section)
}
