#' Store primary analysis results, processed from ExportDatum
#'
#' @slot Source.Well.ID factor of well IDs from source data
#' @slot Internal.Control boolean vector of whether a sample is an internal control
#' @slot Rep1.Well factor of 384 well plate locations for each first replicate
#' @slot Rep2.Well factor of 384 well plate locations for each second replicate
#' @slot Rep3.Well factor of 384 well plate locations for each third replicate
#' @slot Rep1.ExperimentalCt.prefilter numeric vector of raw telomeric Ct values for each first replicate
#' @slot Rep2.ExperimentalCt.prefilter numeric vector of raw telomeric Ct values for each second replicate
#' @slot Rep3.ExperimentalCt.prefilter numeric vector of raw telomeric Ct values for each third replicate
#' @slot Rep1.ControlCt.prefilter numeric vector of raw control Ct values for each first replicate
#' @slot Rep2.ControlCt.prefilter numeric vector of raw control Ct values for each second replicate
#' @slot Rep3.ControlCt.prefilter numeric vector of raw control Ct values for each third replicate
#' @slot Rep1.ExperimentalCt.postfilter numeric vector of post-filtering telomeric Ct values for each first replicate
#' @slot Rep2.ExperimentalCt.postfilter numeric vector of post-filtering telomeric Ct values for each second replicate
#' @slot Rep3.ExperimentalCt.postfilter numeric vector of post-filtering telomeric Ct values for each third replicate
#' @slot Rep1.ControlCt.postfilter numeric vector of post-filtering control Ct values for each first replicate
#' @slot Rep2.ControlCt.postfilter numeric vector of post-filtering control Ct values for each second replicate
#' @slot Rep3.ControlCt.postfilter numeric vector of post-filtering control Ct values for each third replicate
#' @slot Avg.ExperimentalCt numeric vector of mean post-filtering telomeric Ct values
#' @slot SD.ExperimentalCt numeric vector of standard deviation of post-filtering telomeric Ct values
#' @slot PerCV.ExperimentalCt numeric vector of [something] of post-filtering telomeric Ct values
#' @slot Avg.ControlCt numeric vector of mean post-filtering control Ct values
#' @slot SD.ControlCt numeric vector of standard deviation of post-filtering control Ct values
#' @slot PerCV.ControlCt numeric vector of [something] of post-filtering control Ct values
#' @slot Fit.ExperimentalConc numeric vector of exponential fit values from standard curve for telomeric data
#' @slot Fit.ControlConc numeric vector of exponential fit values from standard curve for control data
#' @slot TS.Ratio numeric vector of ratio of telomeric to control fit
#' @slot Normalized.TS numeric vector of TS ratio normalized to [something from controls]
#' @keywords telomeres
#' @examples
#' new("PrimaryAnalysis")
#' 
setClass("PrimaryAnalysis", slots = list(Source.Well.ID = "factor",
                                         Internal.Control = "vector",
                                         Rep1.Well = "factor",
                                         Rep2.Well = "factor",
                                         Rep3.Well = "factor",
                                         Rep1.ExperimentalCt.prefilter = "vector",
                                         Rep2.ExperimentalCt.prefilter = "vector",
                                         Rep3.ExperimentalCt.prefilter = "vector",
                                         Rep1.ControlCt.prefilter = "vector",
                                         Rep2.ControlCt.prefilter = "vector",
                                         Rep3.ControlCt.prefilter = "vector",
                                         Rep1.ExperimentalCt.postfilter = "vector",
                                         Rep2.ExperimentalCt.postfilter = "vector",
                                         Rep3.ExperimentalCt.postfilter = "vector",
                                         Rep1.ControlCt.postfilter = "vector",
                                         Rep2.ControlCt.postfilter = "vector",
                                         Rep3.ControlCt.postfilter = "vector",
                                         Avg.ExperimentalCt = "vector",
                                         SD.ExperimentalCt = "vector",
                                         PerCV.ExperimentalCt = "vector",
                                         Avg.ControlCt = "vector",
                                         SD.ControlCt = "vector",
                                         PerCV.ControlCt = "vector",
                                         Fit.ExperimentalConc = "vector",
                                         Fit.ControlConc = "vector",
                                         TS.Ratio = "vector",
                                         Normalized.TS = "vector"))

#' Constructor for primary analysis class
#'
#' Initializes replicate well assignments to default 384 well plate layout.
#' Note that other slots in class can be set but must be named, and named
#' replacements to well assignments will be overwritten, so if you want to
#' use nonstandard well assignments, construct the class directly with new()
#'
#' @keywords telomeres
#' @export
#' @examples
#' PrimaryAnalysis(Source.Well.ID = rep("", 309))
#'
PrimaryAnalysis <- function(...) {
    obj <- new("PrimaryAnalysis", ...)
    shared.levels <- paste(rep(LETTERS[1:16], each = 24), rep(1:24, 16), sep = "")
    obj@Rep1.Well <- factor(c(paste(LETTERS[seq(2, by = 2, length.out = 7)], rep(2, 7), sep = ""),
                              paste(LETTERS[seq(1, by = 2, length.out = 8)], rep(seq(1, 23, 2), each = 8), sep = "")),
                            levels = shared.levels)
    obj@Rep2.Well <- factor(c(paste(LETTERS[seq(2, by = 2, length.out = 7)], rep(4, 7), sep = ""),
                              paste(LETTERS[seq(1, by = 2, length.out = 8)], rep(seq(2, 24, 2), each = 8), sep = "")),
                            levels = shared.levels)
    obj@Rep3.Well <- factor(c(paste(LETTERS[seq(2, by = 2, length.out = 7)], rep(6, 7), sep = ""),
                              paste(LETTERS[seq(2, by = 2, length.out = 8)], rep(seq(1, 23, 2), each = 8), sep = "")),
                            levels = shared.levels)
    obj
}
