#' Writes the parsed and formatted Rd file to a new file in output_dir
#'
#' @param rd_files list of rd_objects to create md file for
#' @param output_dir path to directory to save new md files
#' @param file_ext either .md or .mdx. extension to use for files, default .md
#' @param sections character vector of sections to include in markdown output, in order
#' @param code_sections character vector of sections to format as code blocks
#' @param skip_sections character vector of sections to skip entirely
#'
#' @export
#'
#' @examples \dontrun{
#' rd_files <- extract_package_rd_content("rdstarlight")
#' output_dir <- file.path("path/to/rdstarlight/docs/content/rd_files")
#'
#' write_md_files(rd_files, output_dir)
#'
#' # Custom sections
#' write_md_files(rd_files, output_dir, sections = c("description", "usage", "examples"))
#' }
write_md_files <- function(
  rd_files,
  output_dir,
  file_ext = ".md",
  sections = c(
    "description",
    "usage",
    "arguments",
    "details",
    "value",
    "examples",
    "references",
    "note",
    "author",
    "source",
    "format",
    "section",
    "subsection"
  ),
  code_sections = c("usage", "examples"),
  skip_sections = c("name", "title", "seealso")
) {
  checkmate::assert_choice(file_ext, c(".md", ".mdx"))

  if (!dir.exists(output_dir)) {
    continue <- readline(paste0(
      "output_dir: ",
      output_dir,
      " does not exists. Would you like to create it? [Y/n]"
    ))

    if (tolower(continue) == "y") {
      dir.create(output_dir, recursive = TRUE)
      message(paste("created output_dir:", output_dir))
    } else {
      stop("Please supply output_dir to path that exists")
    }
  }

  for (file in names(rd_files)) {
    md_content <- format_md(
      content = rd_files[[file]],
      sections = sections,
      code_sections = code_sections,
      skip_sections = skip_sections
    )

    writeLines(
      md_content,
      con = file.path(
        output_dir,
        paste0(tools::file_path_sans_ext(basename(file)), file_ext)
      )
    )
  }
}
