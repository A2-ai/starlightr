#' Extract raw code from an Rd examples section
#'
#' @param examples_section The examples section from an Rd object
#' @return Character string of example code
#' @keywords internal
extract_examples_code <- function(examples_section) {
  extract_code_recursive <- function(el) {
    tag <- attr(el, "Rd_tag")

    if (is.character(el)) {
      return(el)
    }

    if (is.list(el)) {
      # Handle special tags
      if (!is.null(tag)) {
        if (tag == "\\dontrun" || tag == "\\donttest") {
          # Skip dontrun/donttest sections for actual execution
          return("")
        } else if (tag == "\\dontshow") {
          return("")
        }
      }
      # Recurse into children
      parts <- vapply(el, extract_code_recursive, character(1))
      return(paste(parts, collapse = ""))
    }

    ""
  }

  code <- extract_code_recursive(examples_section)
  trimws(code)
}

#' Runs example code and capture output to a file
#'
#' @param pkg_name  name of package to collect Rd objects for
#' @param artifact_output_dir path to directory to save ggplots and gt tables
#' @param text_output_dir path to directory to save text output files
#'
#' @export
#'
#' @examples \dontrun{
#' capture_example_output("pkg", "pkg-docs/public")
#' }
capture_example_output <- function(pkg_name, artifact_output_dir, text_output_dir) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for 'capture_example_outputs()'.")
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required for 'capture_example_outputs()'.")
  }

  # Attempt to load the target package (so its functions can be called)
  tryCatch(
    {
      suppressWarnings(
        library(pkg_name, character.only = TRUE)
      )
    },
    error = function(e) {
      stop("Failed to load package '", pkg_name,
           "'. Make sure it's installed. ", e$message)
    }
  )

  # Get Rd database and extract examples from each file
  rd_db <- tools::Rd_db(pkg_name)
  rd_content <- list()
  for (rd_name in names(rd_db)) {
    rd_obj <- rd_db[[rd_name]]
    examples_section <- get_rd_section(rd_obj, "examples")
    if (!is.null(examples_section)) {
      # Extract code text from examples section
      examples_code <- extract_examples_code(examples_section)
      rd_content[[rd_name]] <- list(examples = examples_code)
    }
  }

  if (!dir.exists(artifact_output_dir)) dir.create(artifact_output_dir, recursive = TRUE)
  if (!dir.exists(text_output_dir)) dir.create(text_output_dir, recursive = TRUE)

  for (fn in names(rd_content)) {
    fn_name <- strsplit(fn, ".Rd")[[1]]

    ex_code <- rd_content[[fn]]$examples

    if (is.null(ex_code) || ex_code == "") next

    # Parse into expressions
    ex_exprs <- tryCatch(
      parse(text = ex_code),
      error = function(e) {
        return(NULL)
      }
    )

    if (is.null(ex_exprs)) next

    env <- new.env(parent = globalenv())
    
    # Track if this is the first write to the text file for this function
    first_write <- TRUE
    
    for (i in seq_along(ex_exprs)) {
      val <- tryCatch(
        withVisible(eval(ex_exprs[i], envir = env)),
        error = function(e) {
          message("Error in example ", i, " for ", fn_name, ": ", e$message)
          return(NULL)
        }
      )

      # If runtime error, skip
      if (is.null(val)) next

      # If expression produced a visible result, check if it's ggplot or gt
      if (val$visible) {
        result <- val$value

        # If it's a ggplot, save PNG
        if (inherits(result, "ggplot")) {
          out_file <- file.path(artifact_output_dir, paste0(fn_name, ".png"))
          tryCatch(
            ggplot2::ggsave(filename = out_file, plot = result, width = 6, height = 4),
            error = function(e) {
              message("  Error saving ggplot for", fn_name, ":", e$message, "\n")
            }
          )
          message("Saved ggplot -> ", out_file)

          # If it's a gt table, save HTML
        } else if (inherits(result, "gt_tbl")) {
          out_file <- file.path(artifact_output_dir, paste0(fn_name, ".html"))
          tryCatch(
            gt::gtsave(result, out_file),
            error = function(e) {
              cat("  Error saving gt table for", fn_name, ":", e$message, "\n")
            }
          )
          message("Saved gt table ->", out_file)
        } else {
          # First write overwrites, subsequent writes append
          out_file <- file.path(text_output_dir, paste0(fn_name, ".txt"))
          printed_text <- utils::capture.output(print(result))
          cat(paste(printed_text, collapse = "\n"), "\n", file = out_file, append = !first_write)
          
          if (first_write) {
            message("WROTE output to -> ", out_file)
            first_write <- FALSE
          } else {
            message("APPENDED output to -> ", out_file)
          }
        }
      }
    }
  }
}
