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
capture_example_output <- function(
  pkg_name,
  artifact_output_dir,
  text_output_dir
) {
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
      stop(
        "Failed to load package '",
        pkg_name,
        "'. Make sure it's installed. ",
        e$message
      )
    }
  )

  rd_content <- extract_package_rd_content(pkg_name)

  if (!dir.exists(artifact_output_dir)) dir.create(artifact_output_dir)
  if (!dir.exists(text_output_dir)) dir.create(text_output_dir)

  for (fn in names(rd_content)) {
    # Strip the .Rd suffix from fn to create a cleaner file base name
    fn_name <- sub("\\.Rd$", "", fn)

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
            ggplot2::ggsave(
              filename = out_file,
              plot = result,
              width = 6,
              height = 4
            ),
            error = function(e) {
              message(
                "  Error saving ggplot for",
                fn_name,
                ":",
                e$message,
                "\n"
              )
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
          out_file <- file.path(text_output_dir, paste0(fn_name, ".txt"))
          printed_text <- utils::capture.output(print(result))
          writeLines(printed_text, out_file)
          message("Saved generic output -> ", out_file)
        }
      }
    }
  }
}
