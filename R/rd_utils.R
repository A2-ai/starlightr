# Core Rd parsing utilities for starlightr

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
