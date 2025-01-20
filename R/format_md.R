#' Formats the rd file contents to markdown
#'
#' @param content rd_file contents
#'
#' @return markdown formatted string of content
#' @export
#'
#' @examples \dontrun{
#' rd_files <- extract_package_rd_contents("rdstarlight")
#' format_md(rd_files[["function_name.Rd"]])
#' }
format_md <- function(content) {
  arguments_table <- if (!is.null(content$arguments)) {
    args_df <- process_arguments(content$arguments)
    paste0(
      "| Name   | Description |\n",
      "|--------|-------------|\n",
      paste(
        apply(args_df, 1, function(row) paste0("|`", row["name"], "`| ", row["description"], "|")),
        collapse = "\n"
      )
    )
  } else {
    "No arguments documented."
  }

  mdx <- paste0(
    "## Description\n\n", content$description, "\n",
    "## Usage\n\n```r\n", content$usage, "\n```\n",
    "## Arguments\n\n", arguments_table, "\n",
    "## Returns\n\n", content$value, "\n",
    "## Examples\n\n```r\n", content$examples, "\n```"
  )

  return(mdx)
}
