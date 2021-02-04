#' Process a CGR-style telomere qPCR experiment
#'
#' @description
#' `process.experiment` takes the output of a lab telomere qPCR experiment
#' and generates the expected output content from prior processing methods.
#'
#' @details
#' This is the main entry point for the CGR telomere processing package.
#' The input directory is sanity checked for its expected contents. The
#' output directory will be created if it doesn't already exist. Any
#' existing conflicting output files will be overwritten by this method,
#' so please change output.path if you don't want this behavior.
#'
#' During development, plate content report and list files were not
#' necessarily available for test cases. As such, an override mode
#' has been added that pulls pre-pasted copies of relevant content
#' report data from the spreadsheets in Data/Analysis from an existing
#' telomere run. During deployment, this will almost certainly not be
#' the desired behavior, and this feature is scheduled to be removed.
#' This flag will also be overridden when plate list and content report
#' are both specified.
#'
#' In all existing test datasets, a fixed mapping from 96 well to 384 well
#' layouts has been found. For mild efficiency purposes, this mapping
#' has been set as the default. If there is ever any doubt, or if the
#' well mapping is changed at some point in the future, setting
#' `infer.384.locations` to `FALSE` will trigger dynamic remapping using
#' the data in `plate.content.report` and `plate.list`.
#'
#' @param input.path Character vector of input directory path
#' @param output.path Character vector of output directory path
#' @param project.id Character vector of project ID
#' (e.g. "GP0317-TL7") or NA
#' @param plate.content.report Character vector of plate content
#' report for project (e.g. "PlateContentReport_GP0317-TL1.xls") or NA
#' @param plate.list Character vector of plate list for project
#' (e.g. "PlateList_GP0317-TL1.xls") or NA
#' @param infer.384.locations Logical: whether to assume fixed
#' 96->384 well mapping
#' @param subject.list.from.input.path Logical: whether to pull subject
#' data from Data/Analysis/*xlsx of an existing analysis run
#' @param control.vials character vector: internal control vial IDs
#' @return TBD
#' @keywords telomeres
#' @export process.experiment
#' @examples
#' process.experiment(
#'   "Examples for Bioinformatics",
#'   "C:/Users/palmercd/Documents/telomeres/output", "GP0317-TL7"
#' )
process.experiment <- function(input.path,
                               output.path,
                               project.id = NA,
                               plate.content.report = NA,
                               plate.list = NA,
                               infer.384.locations = FALSE,
                               subject.list.from.input.path = TRUE,
                               control.vials = c("NA07057")) {
  final.tsv <- paste(output.path,
    "Analysis",
    "Final",
    paste(project.id, " Results.tsv", sep = ""),
    sep = "/"
  )
  markdown.html <- paste(output.path,
    "Analysis",
    "Final",
    paste(project.id, " QC Report.html", sep = ""),
    sep = "/"
  )
  ## check parameter requirements
  ## type checks
  stopifnot(is.vector(input.path, mode = "character") &
    dir.exists(input.path))
  stopifnot(is.vector(output.path, mode = "character"))
  stopifnot(is.vector(project.id, mode = "character") |
    isTRUE(is.na(project.id)))
  stopifnot(is.vector(plate.content.report, mode = "character") |
    isTRUE(is.na(plate.content.report)))
  if (is.vector(plate.content.report, mode = "character")) {
    stopifnot(file.exists(plate.content.report))
  }
  stopifnot(is.vector(plate.list, mode = "character") |
    isTRUE(is.na(plate.list)))
  if (is.vector(plate.list, mode = "character")) {
    stopifnot(file.exists(plate.list))
  }
  stopifnot(is.logical(infer.384.locations))
  stopifnot(is.logical(subject.list.from.input.path))
  ## `!subject.list.from.input.path` only works if the plate list
  ## and content report are both specified
  stopifnot((!is.na(plate.content.report) & !is.na(plate.list)) |
    subject.list.from.input.path)
  ## `infer.384.locations` only works if the plate list and content
  ## report are both specified
  stopifnot(!infer.384.locations | (infer.384.locations &
    !is.na(plate.content.report) &
    !is.na(plate.list)))
  stopifnot(is.vector(control.vials, mode = "character"))
  ## find the installed template Rmd file
  rmarkdown.template <- system.file("rmd", "report.Rmd",
    package = "cgrtelomeres"
  )
  rmarkdown::render(rmarkdown.template,
    output_file = markdown.html,
    output_dir = ".",
    params = list(
      input.path = input.path,
      output.path = output.path,
      project.id = project.id,
      plate.content.report = plate.content.report,
      plate.list = plate.list,
      final.tsv = final.tsv,
      infer.384.locations = infer.384.locations,
      subject.list.from.input.path =
        subject.list.from.input.path
    )
  )
}
