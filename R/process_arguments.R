#' Formats function arguments into markdown table
#'
#' @param arguments_text text of arguments from rd_obj
#'
#' @return dataframe of arguments tables with name and description columns
#' @export
#'
#' @examples \dontrun{
#' rd_files <- extract_package_rd_content("starlightr")
#' process_arguments(rd_files[['write_md_files.Rd']]$arguments)
#' }
process_arguments <- function(arguments_text) {
  if (is.null(arguments_text) || nchar(trimws(arguments_text)) == 0) {
    return(data.frame(
      name = character(0),
      description = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Clean up the text
  cleaned_text <- trimws(arguments_text)

  # R documentation arguments are typically separated by multiple newlines/spaces
  # Split on patterns like "\n \n" which separate different arguments
  arg_chunks <- unlist(strsplit(
    cleaned_text,
    "\\s*\\n\\s*\\n\\s*",
    perl = TRUE
  ))

  # Remove empty chunks
  arg_chunks <- arg_chunks[nchar(trimws(arg_chunks)) > 0]

  if (length(arg_chunks) == 0) {
    return(data.frame(
      name = character(0),
      description = character(0),
      stringsAsFactors = FALSE
    ))
  }

  args_list <- list()

  for (chunk in arg_chunks) {
    # Each chunk should start with the argument name followed by description
    # Clean up the chunk
    chunk <- trimws(chunk)
    chunk <- gsub("\\s+", " ", chunk) # Replace multiple spaces with single space

    # Split on first space to separate name from description
    words <- unlist(strsplit(chunk, "\\s+", perl = TRUE))

    if (length(words) >= 2) {
      name <- words[1]
      description <- paste(words[-1], collapse = " ")

      # Clean up common artifacts in descriptions
      description <- trimws(description)

      args_list[[length(args_list) + 1]] <- c(
        name = name,
        description = description
      )
    } else if (length(words) == 1) {
      # Single word - might be just an argument name
      args_list[[length(args_list) + 1]] <- c(name = words[1], description = "")
    }
  }

  if (length(args_list) > 0) {
    args_df <- as.data.frame(
      do.call(rbind, args_list),
      stringsAsFactors = FALSE
    )
    names(args_df) <- c("name", "description")
    return(args_df)
  }

  # Fallback: return as single entry
  return(data.frame(
    name = "arguments",
    description = cleaned_text,
    stringsAsFactors = FALSE
  ))
}
