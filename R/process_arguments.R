#' Formats function arguments into markdown table
#'
#' @param arguments_text text of arguments from rd_obj
#'
#' @return dataframe of arguments tables with name and description columns
#' @export
#'
#' @examples \dontrun{
#' rd_files <- extract_package_rd_contents("rdstarlight")
#' process_arguments(rd_files[['write_md_files.Rd']]$arguments)
#' }
process_arguments <- function(arguments_text) {
  if (is.null(arguments_text)) return(data.frame())

  # Remove leading/trailing whitespace and normalize newlines
  cleaned_text <- gsub("^\\s+|\\s+$", "", arguments_text)
  cleaned_text <- gsub("\n", " ", cleaned_text)

  # Split on double spaces to separate argument entries
  split_args <- unlist(strsplit(cleaned_text, " {2,}"))

  # Parse each argument into name and description
  args_list <- lapply(split_args, function(arg) {
    parts <- unlist(strsplit(arg, " ", fixed = TRUE))
    name <- parts[1]
    description <- paste(parts[-1], collapse = " ")
    c(name = name, description = description)
  })

  # Convert list to data frame
  args_df <- as.data.frame(do.call(rbind, args_list), stringsAsFactors = FALSE)
  names(args_df) <- c("name", "description")

  return(args_df)
}
