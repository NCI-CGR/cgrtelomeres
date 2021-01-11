#' Acquire input text files in (experiment, control) pairs
#'
#' @description
#' `find.input.files` scans a given directory for structured
#' output from a lab telomere experiment and organizes them
#' by experiment, control pairs.
#'
#' @details
#' This function aggregates filenames and performs mild
#' error checking on the existence of expected pairs of
#' filenames. It will ignore any file present in the specified
#' directory that does not match the expected general filename
#' format. Paths should be specified using the R convention of
#' "/" as the delimiter between directories (as opposed to the
#' Windows convention of "\"). This function should not be
#' called directly by the end user.
#'
#' @param path Character vector describing path to input
#' file directory
#' @param telo.tag Character vector describing file tag
#' for experimental condition
#' @param control.tag Character vector describing file
#' tag for control condition
#' @return list of pairs of (telomere, control) files
#' @keywords telomeres
#' @examples
#' find.input.files("/path/to/Data/Exports")
find.input.files <- function(path,
                             telo.tag = "Telo",
                             control.tag = "36B4") {
  telo.files <- list.files(
    path,
    paste("^[A-Z0-9]+_[A-Z]_",
      telo.tag,
      "\\.txt$",
      sep = ""
    )
  )
  control.files <- list.files(
    path,
    paste("^[A-Z0-9]+_[A-Z]_",
      control.tag,
      "\\.txt$",
      sep = ""
    )
  )
  stopifnot(length(telo.files) == length(control.files))
  lapply(seq_len(length(telo.files)), function(i) {
    ## paired files appear to be named
    ## [A-Z]+([0-9]+)_([A-Z])_36B4.txt and
    ## [A-Z]+{\1+1}_\2_Telo.txt
    expected.telo <- paste(substr(control.files[i], 1, 2),
      as.numeric(gsub(
        "^[A-Z]+([0-9]+)_.*$", "\\1",
        control.files[i]
      )) + 1,
      gsub(
        "^[A-Z0-9]+(_[A-Z]_).*$", "\\1",
        control.files[i]
      ),
      telo.tag,
      ".txt",
      sep = ""
    )
    stopifnot(telo.files[i] == expected.telo)
    c(
      paste(path, telo.files[i], sep = "/"),
      paste(path, control.files[i], sep = "/")
    )
  })
}
