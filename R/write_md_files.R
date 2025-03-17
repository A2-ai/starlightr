#' Writes the parsed and formatted Rd file to a new file in output_dir
#'
#' @param rd_files list of rd_objects to create md file for
#' @param output_dir path to directory to save new md files
#' @param file_ext either .md or .mdx. extension to use for files, default .md
#'
#' @export
#'
#' @examples \dontrun{
#' rd_files <- extract_package_rd_contents("rdstarlight")
#' output_dir <- file.path("path/to/rdstarlight/docs/content/rd_files")
#'
#' write_md_files(rd_files, output_dir)
#' }
write_md_files <- function(rd_files, output_dir, file_ext = ".md") {
  checkmate::assert_choice(file_ext, c(".md", ".mdx"))
  if (!dir.exists(output_dir)) {
    continue <- readLines(paste0(
      "output_dir: ", output_dir, " does not exists. Would you like to create it? [Y/n]"
      )
    )

    if (tolower(continue) == "y") {
      dir.create(output_dir, recursive = TRUE)
    } else {
      stop("Please supply output_dir to path that exists")
    }
  }

  for (file in names(rd_files)) {
    md_content <- format_md(rd_files[[file]])

    writeLines(
      md_content,
      con = file.path(
        output_dir,
        paste0(tools::file_path_sans_ext(basename(file)), file_ext)
      )
    )
  }
}
