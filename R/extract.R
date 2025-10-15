#' Pulls out text from rd_obj if Rd_tag attribute matches supplied tag
#'
#' @param rd_obj rd object from tools::Rd_db
#' @param tag Rd section like name, description, arguments...
#'
#' @return a string of the tagged section
#' @export
#'
#' @examples \dontrun{
#' rd_files <- tools::Rd_db("starlightr")
#'
#' extract_section(rd_files[["extract_section.Rd"]], "description")
#' }
extract_section <- function(rd_obj, tag) {
  if (!startsWith(tag, "\\")) {
    tag <- paste0("\\", tag)
  }

  for (element in rd_obj) {
    if (attr(element, "Rd_tag") == tag) {
      return(paste(unlist(element), collapse = " "))
    }
  }
  return(NULL) # Return NULL if the tag is not found
}

#' Get all available Rd tags from an rd object
#'
#' @param rd_obj rd object from tools::Rd_db
#'
#' @return character vector of all Rd_tag values found in the object
#' @export
get_rd_tags <- function(rd_obj) {
  tags <- character(0)
  for (element in rd_obj) {
    tag <- attr(element, "Rd_tag")
    if (!is.null(tag) && !tag %in% tags) {
      tags <- c(tags, tag)
    }
  }
  return(tags)
}

#' Extracts all sections from an Rd object dynamically
#'
#' @param rd_obj rd object from tools::Rd_db
#' @param include_common_only logical, if TRUE only extracts common sections,
#'   if FALSE extracts all available sections
#'
#' @return list of sections for the rd_obj
#' @export
#'
#' @examples \dontrun{
#' rd_files <- tools::Rd_db("starlightr")
#'
#' # Extract all sections
#' extract_rd_content(rd_files[["extract_section.Rd"]])
#'
#' # Extract only common sections
#' extract_rd_content(rd_files[["extract_section.Rd"]], include_common_only = TRUE)
#' }
extract_rd_content <- function(rd_obj, include_common_only = FALSE) {
  # Common sections that are typically found
  common_sections <- c(
    "\\name",
    "\\description",
    "\\usage",
    "\\arguments",
    "\\value",
    "\\examples",
    "\\details",
    "\\seealso",
    "\\references",
    "\\note",
    "\\author",
    "\\source",
    "\\format",
    "\\section",
    "\\subsection"
  )

  if (include_common_only) {
    # Only extract common sections
    sections_to_extract <- common_sections
  } else {
    # Get all available tags in this specific rd object
    available_tags <- get_rd_tags(rd_obj)
    sections_to_extract <- available_tags
  }

  # Extract content for each section
  content <- list()
  for (tag in sections_to_extract) {
    section_content <- extract_section(rd_obj, tag)
    if (!is.null(section_content)) {
      # Clean up the tag name for the list (remove backslash)
      clean_tag <- gsub("^\\\\", "", tag)
      content[[clean_tag]] <- section_content
    }
  }

  return(content)
}

#' Wrapper for backward compatibility - extracts only common sections
#'
#' @param rd_obj rd object from tools::Rd_db
#'
#' @return list of common sections for the rd_obj
#' @export
extract_rd_content_common <- function(rd_obj) {
  list(
    title = extract_section(rd_obj, "\\name"),
    description = extract_section(rd_obj, "\\description"),
    usage = extract_section(rd_obj, "\\usage"),
    arguments = extract_section(rd_obj, "\\arguments"),
    value = extract_section(rd_obj, "\\value"),
    examples = extract_section(rd_obj, "\\examples")
  )
}

#' extracts all Rd files from a package and retrieves each section
#' from extract_rd_content
#'
#' @param pkg_name name of package to collect Rd objects for
#' @param include_common_only logical, if TRUE only extracts common sections,
#'   if FALSE extracts all available sections
#'
#' @return list of rd_objects, with each section parsed
#' @export
#'
#' @examples
#' # Extract all sections
#' extract_package_rd_content("starlightr")
#'
#' # Extract only common sections
#' extract_package_rd_content("starlightr", include_common_only = TRUE)
extract_package_rd_content <- function(pkg_name, include_common_only = FALSE) {
  rd_files <- tools::Rd_db(pkg_name)
  return(lapply(
    rd_files,
    function(x) extract_rd_content(x, include_common_only)
  ))
}
