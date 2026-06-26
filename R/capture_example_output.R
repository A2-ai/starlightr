#' Extract raw code from an Rd examples section
#'
#' @param examples_section The examples section from an Rd object
#' @return Character string of example code
#' @keywords internal
#' @noRd
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

#' Capture example outputs from Rd files in memory
#'
#' Runs @examples sections from the specified functions and returns captured
#' results (ggplot objects, gt tables, text output) as an in-memory list.
#'
#' @param pkg_name Name of the installed package
#' @param fn_names Character vector of function names to capture examples for.
#'   These correspond to .Rd filenames without extension.
#' @param verbose Logical, whether to print debug messages (default FALSE)
#'
#' @return A named list keyed by function name. Each element is a list with
#'   optional components:
#'   \describe{
#'     \item{txt}{Character string of captured text output}
#'     \item{png_raw}{Raw vector of PNG image data}
#'     \item{html}{Character string of rendered HTML (gt tables)}
#'   }
#' @keywords internal
#' @noRd
capture_rd_examples <- function(pkg_name, fn_names, verbose = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for capturing example outputs.")
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required for capturing example outputs.")
  }

  # Load the target package
  tryCatch(
    {
      suppressWarnings(
        library(pkg_name, character.only = TRUE)
      )
    },
    error = function(e) {
      stop(
        "Failed to load package '",
        pkg_name,
        "'. Make sure it's installed. ",
        e$message
      )
    }
  )

  # Get Rd database and filter to requested functions
  rd_db <- tools::Rd_db(pkg_name)
  rd_content <- list()
  for (rd_name in names(rd_db)) {
    fn_name <- tools::file_path_sans_ext(rd_name)
    if (!fn_name %in% fn_names) next

    rd_obj <- rd_db[[rd_name]]
    examples_section <- get_rd_section(rd_obj, "examples")
    if (!is.null(examples_section)) {
      examples_code <- extract_examples_code(examples_section)
      rd_content[[fn_name]] <- examples_code
    }
  }

  results <- list()

  for (fn_name in names(rd_content)) {
    ex_code <- rd_content[[fn_name]]
    if (is.null(ex_code) || ex_code == "") next

    ex_exprs <- tryCatch(
      parse(text = ex_code),
      error = function(e) NULL
    )
    if (is.null(ex_exprs)) next

    if (verbose) {
      message("Processing examples for: ", fn_name)
      message("Number of expressions: ", length(ex_exprs))
      message("Code:\n", ex_code, "\n---")
    }

    eval_env <- new.env(parent = globalenv())
    entry <- list()
    txt_parts <- character()

    for (i in seq_along(ex_exprs)) {
      if (verbose) {
        message("  Evaluating expression ", i, ": ", deparse(ex_exprs[[i]])[1])
      }
      val <- tryCatch(
        withVisible(eval(ex_exprs[[i]], envir = eval_env)),
        error = function(e) {
          message("  Error in example ", i, " for ", fn_name, ": ", e$message)
          NULL
        }
      )

      if (is.null(val)) next

      if (val$visible) {
        result <- val$value

        if (inherits(result, "ggplot")) {
          tmp <- tempfile(fileext = ".png")
          tryCatch(
            {
              ggplot2::ggsave(
                filename = tmp,
                plot = result,
                width = 6,
                height = 4
              )
              entry$png_raw <- readBin(tmp, "raw", file.info(tmp)$size)
            },
            error = function(e) {
              message("  Error saving ggplot for ", fn_name, ": ", e$message)
            }
          )
          unlink(tmp)
          if (verbose) message("  Captured ggplot for ", fn_name)
        } else if (inherits(result, "gt_tbl")) {
          tryCatch(
            {
              entry$html <- gt::as_raw_html(result)
            },
            error = function(e) {
              message("  Error rendering gt table for ", fn_name, ": ", e$message)
            }
          )
          if (verbose) message("  Captured gt table for ", fn_name)
        } else {
          printed_text <- utils::capture.output(print(result))
          txt_parts <- c(txt_parts, paste(printed_text, collapse = "\n"))
        }
      }
    }

    if (length(txt_parts) > 0) {
      entry$txt <- paste(txt_parts, collapse = "\n")
    }

    if (length(entry) > 0) {
      results[[fn_name]] <- entry
    }
  }

  results
}
