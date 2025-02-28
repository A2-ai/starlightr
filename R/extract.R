#' Pulls out text from rd_obj if Rd_tag attribute matches supplied tag
#'
#' @param rd_obj rd object from tools::Rd_db
#' @param tag Rd section like name, description, arguments...
#'
#' @return a string of the tagged section
#' @export
#'
#' @examples \dontrun{
#' rd_files <- tools::Rd_db("rdstarlight")
#'
#' extract_section(rd_files[['extract_section.Rd']], "description")
#' }
extract_section <- function(rd_obj, tag) {
  if (!startsWith(tag, "\\")) {
    tag <- paste0('\\', tag)
  }

  for (element in rd_obj) {
    if (attr(element, "Rd_tag") == tag) {
      return(paste(unlist(element), collapse = " "))
    }
  }
  return(NULL) # Return NULL if the tag is not found
}

#' Wraps extract_section for name, description, usage, arguments, value, examples
#'
#' @param rd_obj rd object from tools::Rd_db
#'
#' @return list of sections for the rd_obj
#' @export
#'
#' @examples \dontrun{
#' rd_files <- tools::Rd_db("rdstarlight")
#'
#' extract_rd_content(rd_files[['extract_section.Rd']])
#' }
extract_rd_content <- function(rd_obj) {
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
#' from extract_rd_section
#'
#' @param pkg_name name of package to collect Rd objects for
#'
#' @return list of rd_objects, with each section parsed
#' @export
#'
#' @examples
#' extract_package_rd_content("rdstarlight")
extract_package_rd_content <- function(pkg_name) {
  rd_files <- tools::Rd_db(pkg_name)
  return(lapply(rd_files, extract_rd_content))
}
