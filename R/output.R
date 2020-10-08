#' Create output directory structure if necessary
#'
#' @description
#' `create.output.directories` creates all directories and subdirectories
#' required for the standard output of the telomere protocol. If the top-level
#' directory does not exist, it will also be created.
#' 
#' @details
#' The output directories are: top/Data/Analysis, top/Analysis/QC, top/Analysis/Final.
#' This silently handles if said folders already exist.
#'
#' @param top.path Character vector of output top-most directory
#' @keywords telomeres
#' @examples
#' create.output.directories("C:/Users/palmercd/Documents/telomeres/results")
#' 
create.output.directories <- function(top.path) {
    dir.create(top.path, showWarnings = FALSE)
    dir.create(paste(top.path, "Data", sep = "/"), showWarnings = FALSE)
    dir.create(paste(top.path, "Data", "Analysis", sep = "/"), showWarnings = FALSE)
    dir.create(paste(top.path, "Analysis", sep = "/"), showWarnings = FALSE)
    dir.create(paste(top.path, "Analysis", "QC", sep = "/"), showWarnings = FALSE)
    dir.create(paste(top.path, "Analysis", "Final", sep = "/"), showWarnings = FALSE)
}

#' Write PrimaryAnalysis object to file in simplified format
#'
#' @description
#' `report.primary.analysis` takes a processed PrimaryAnalysis object
#' and reports it to file in a format analogous to the report format
#' from the existing excel template.
#' 
#' @details
#' Exact format details are TBD.
#'
#' @param primary.analysis PrimaryAnalysis object to be written to file
#' @param output.path Character vector of output directory path
#' @param project.id Character vector of project ID (e.g. "GP0317-TL7")
#' @keywords telomeres
#' @seealso [create.analysis()] for generating compatible PrimaryAnalysis objects.
#' @examples
#' my.files <- find.input.files("Data/Exports")
#' my.data <- read.export.datum(my.files[[1]])
#' my.analysis <- create.analysis(my.data)
#' report.primary.analysis(my.analysis, "my.output", "GP0317-TL7")
#'
report.primary.analysis <- function(primary.analysis, output.path, project.id) {
    ## construct output filename from output path, project ID, analysis code
    output.filename <- paste(output.path, "/Data/Analysis/", project.id, "_",  primary.analysis@Analysis.Code, ".tsv", sep = "")
    ## combine data in a data frame for reporting convenience
    output.df <- data.frame(primary.analysis@Source.Well.ID,
                            primary.analysis@Rep1.Well,
                            primary.analysis@Rep2.Well,
                            primary.analysis@Rep3.Well,
                            primary.analysis@Rep1.ExperimentalCt.prefilter,
                            primary.analysis@Rep2.ExperimentalCt.prefilter,
                            primary.analysis@Rep3.ExperimentalCt.prefilter,
                            primary.analysis@Rep1.ControlCt.prefilter,
                            primary.analysis@Rep2.ControlCt.prefilter,
                            primary.analysis@Rep3.ControlCt.prefilter,
                            primary.analysis@Rep1.ExperimentalCt.postfilter,
                            primary.analysis@Rep2.ExperimentalCt.postfilter,
                            primary.analysis@Rep3.ExperimentalCt.postfilter,
                            primary.analysis@Rep1.ControlCt.postfilter,
                            primary.analysis@Rep2.ControlCt.postfilter,
                            primary.analysis@Rep3.ControlCt.postfilter,
                            primary.analysis@Avg.ExperimentalCt,
                            primary.analysis@SD.ExperimentalCt,
                            primary.analysis@PerCV.ExperimentalCt,
                            primary.analysis@Avg.ControlCt,
                            primary.analysis@SD.ControlCt,
                            primary.analysis@PerCV.ControlCt,
                            primary.analysis@Fit.ExperimentalConc,
                            primary.analysis@Fit.ControlConc,
                            primary.analysis@TS.Ratio)
    ## set the column names to approximately match those currently in the excel template
    colnames(output.df) <- c("Source Well ID",
                             "Rep 1 Well",
                             "Rep 2 Well",
                             "Rep 3 Well",
                             "Rep 1 Telo Ct",
                             "Rep 2 Telo Ct",
                             "Rep 3 Telo Ct",
                             "Rep 1 36B4 Ct",
                             "Rep 2 36B4 Ct",
                             "Rep 3 36B4 Ct",
                             "Rep 1 Telo Ct filtered",
                             "Rep 2 Telo Ct filtered",
                             "Rep 3 Telo Ct filtered",
                             "Rep 1 36B4 Ct filtered",
                             "Rep 2 36B4 Ct filtered",
                             "Rep 3 36B4 Ct filtered",
                             "Avg Telo Ct",
                             "Telo StDev",
                             "Telo Ct %CV",
                             "Avg 36B4 Ct",
                             "36B4 StDev",
                             "36B4 Ct %CV",
                             "[Telo] from ER",
                             "[36B4] from ER",
                             "T/S from ER")
    ## report data to file
    write.table(output.df, output.filename, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
}
