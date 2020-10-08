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
#' @slot Model.Experiment object of class lm, fit model for telomeric data
#' @slot Model.Control object of class lm, fit model for control data
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
                                         Model.Experiment = "lm",
                                         Model.Control = "lm",
                                         Fit.ExperimentalConc = "vector",
                                         Fit.ControlConc = "vector",
                                         TS.Ratio = "vector",
                                         Normalized.TS = "vector"))

#' Constructor for primary analysis class
#'
#' Initializes replicate well assignments to default 384 well plate layout
#' and standard configuration of 96 well source.
#' Note that other slots in class can be set but must be named, and named
#' replacements to well assignments will be overwritten, so if you want to
#' use nonstandard well assignments, construct the class directly with new()
#'
#' @keywords telomeres
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
    obj@Source.Well.ID <- factor(c(rep("Standards", 6),
                                   "NTC",
                                   paste(rep(LETTERS[1:8], 12), sprintf("%02d", rep(1:12, each = 8)), sep = "-")))
}

#' Apply Ct filtering to PrimaryAnalysis intermediates based on some criteria
#'
#' @description
#' `apply.Ct.filtering` takes raw Ct(Cp) values and applies mild postprocessing
#' to conform with current behavior of the analysis protocol.
#'
#' @details
#' The current filter applied is "Ct equal to 38 is set to NA". This is different
#' than what's applied in the excel template (equal to 28, set to 0), but based on
#' how the Cts are defined, the 28 criterion is actually a bug in the template. By
#' inspection, it seems like the actual current protocol is to go through and
#' manually blank entries that exceed some Ct threshold, possibly closer to 30.
#' This function provides centralized control should that filter need to be
#' adjusted in the future.
#' 
#' @param vec Numeric vector of raw Ct values.
#' @return Numeric vector of input with some overlarge values set to NA.
#' @keywords telomeres
#' @seealso [read.export.datum()] for acquiring raw Ct values, [PrimaryAnalysis()]
#' for construction of PrimaryAnalysis objects that use these processed Cts.
#' @examples
#' filtered.data <- apply.Ct.filtering(raw.data)
#'
apply.Ct.filtering <- function(vec) {
    res <- vec
    res[abs(res - 38) < 1.5e-8] <- NA
    res
}

#' Compute replicate mean with restrictions
#'
#' @description
#' `process.average` takes three replicates' data and combines vector-wise
#' into means while allowing a single observation from each condition at
#' most to be missing (flagged as NA).
#'
#' @details
#' There is an implicit restriction that appears to be applied manually to the
#' real data example, where conditions with fewer than two replicates are blanked.
#' Hopefully, this should cover that situation, while allowing single dropouts.
#'
#' @param vec.rep1 numeric vector of first replicate Ct data
#' @param vec.rep2 numeric vector of second replicate Ct data
#' @param vec.rep3 numeric vector of third replicate Ct data
#' @return numeric vector of vector-wise averages, with NA exclusion criteria applied
#' @seealso [process.standard.deviation()] for equivalent calculation with standard deviation.
#' @examples
#' vec.mean <- process.average(rnorm(10), rnorm(10), rnorm(10))
#' 
process.average <- function(vec.rep1,
                            vec.rep2,
                            vec.rep3) {
    apply(cbind(vec.rep1, vec.rep2, vec.rep3),
          1,
          function (i) {
              ifelse(length(which(!is.na(i))) < 2, mean(i, na.rm = TRUE), NA)
          })
}

#' Compute replicate standard deviation with restrictions
#'
#' @description
#' `process.standard.deviation` takes three replicates' data and combines vector-wise
#' into standard deviations while allowing a single observation from each condition
#' at most to be missing (flagged as NA).
#'
#' @details
#' There is an implicit restriction that appears to be applied manually to the
#' real data example, where conditions with fewer than two replicates are blanked.
#' Hopefully, this should cover that situation, while allowing single dropouts.
#'
#' @param vec.rep1 numeric vector of first replicate Ct data
#' @param vec.rep2 numeric vector of second replicate Ct data
#' @param vec.rep3 numeric vector of third replicate Ct data
#' @return numeric vector of vector-wise averages, with NA exclusion criteria applied
#' @seealso [process.average()] for equivalent calculation with mean
#' @examples
#' vec.sd <- process.standard.deviation(rnorm(10), rnorm(10), rnorm(10))
#' 
process.standard.deviation <- function(vec.rep1,
                                       vec.rep2,
                                       vec.rep3) {
    apply(cbind(vec.rep1, vec.rep2, vec.rep3),
          1,
          function (i) {
              ifelse(length(which(!is.na(i))) < 2, sd(i, na.rm = TRUE), NA)
          })
}

#' Create a PrimaryAnalysis from a loaded ExportDatum
#'
#' @description
#' `create.analysis` takes an ExportDatum object containing telomeric
#' and control data and applies the standard analysis currently being run
#' by the excel template that generates files in Data/Analysis/*xlsx.
#'
#' @details
#' This function attempts to emulate the results in the primary analysis spreadsheet.
#' There's a fair amount of redundant information (log2 transform of some data, linear
#' regression information that is deprecated) that is not being included for the
#' moment. 
#'
#' @param export.datum an ExportDatum object with raw qPCR data for processing
#' @return a PrimaryAnalysis object containing the processed results for the input
#' @keywords telomeres
#' @export
#' @seealso [PrimaryAnalysis()] for constructing empty PrimaryAnalysis objects;
#' [read.export.datum()] for generating compatible input data structures.
#' @examples
#'
create.analysis <- function(export.datum) {
    obj <- PrimaryAnalysis()
    ## load Cp data from export datum at the appropriate wells
    obj@Rep1.ExperimentalCt.prefilter <- export.datum@Cp.Telo[obj@Rep1.Well]
    obj@Rep2.ExperimentalCt.prefilter <- export.datum@Cp.Telo[obj@Rep2.Well]
    obj@Rep3.ExperimentalCt.prefilter <- export.datum@Cp.Telo[obj@Rep3.Well]
    obj@Rep1.ControlCt.prefilter <- export.datum@Cp.Control[obj@Rep1.Well]
    obj@Rep2.ControlCt.prefilter <- export.datum@Cp.Control[obj@Rep2.Well]
    obj@Rep3.ControlCt.prefilter <- export.datum@Cp.Control[obj@Rep3.Well]
    ## apply filtering based on *some criterion*
    obj@Rep1.ExperimentalCt.postfilter <- apply.Ct.filtering(obj@Rep1.ExperimentalCt.prefilter)
    obj@Rep2.ExperimentalCt.postfilter <- apply.Ct.filtering(obj@Rep2.ExperimentalCt.prefilter)
    obj@Rep3.ExperimentalCt.postfilter <- apply.Ct.filtering(obj@Rep3.ExperimentalCt.prefilter)
    obj@Rep1.ControlCt.postfilter <- apply.Ct.filtering(obj@Rep1.ControlCt.prefilter)
    obj@Rep2.ControlCt.postfilter <- apply.Ct.filtering(obj@Rep2.ControlCt.prefilter)
    obj@Rep3.ControlCt.postfilter <- apply.Ct.filtering(obj@Rep3.ControlCt.prefilter)
    ## compute average, SD, percent coefficient of variation for each condition.
    ## just by inspection, it seems like conditions with fewer than two successful
    ## replicates are blanked out.
    obj@Avg.ExperimentalCt <- process.average(obj@Rep1.ExperimentalCt.postfilter,
                                              obj@Rep2.ExperimentalCt.postfilter,
                                              obj@Rep3.ExperimentalCt.postfilter)
    obj@SD.ExperimentalCt <- process.standard.deviation(obj@Rep1.ExperimentalCt.postfilter,
                                                        obj@Rep2.ExperimentalCt.postfilter,
                                                        obj@Rep3.ExperimentalCt.postfilter)
    obj@PerCV.ExperimentalCt <- 100 * obj@SD.ExperimentalCt / obj@Avg.ExperimentalCt
    obj@Avg.ControlCt <- process.average(obj@Rep1.ControlCt.postfilter,
                                         obj@Rep2.ControlCt.postfilter,
                                         obj@Rep3.ControlCt.postfilter)
    obj@SD.ControlCt <- process.standard.deviation(obj@Rep1.ControlCt.postfilter,
                                                   obj@Rep2.ControlCt.postfilter,
                                                   obj@Rep3.ControlCt.postfilter)
    obj@PerCV.ControlCt <- 100 * obj@SD.ControlCt / obj@Avg.ControlCt
    ## must run exponential regressions against the standard lanes.
    ## now, it *appears* that the standard concentrations are predefined in the excel template,
    ## and are likely fixed across experiments. set these here for now; possibly expose them later.
    ## note that this model requires post hoc transformation before you get the exact coefficients
    ## that were being produced with excel's `logest`
    fixed.concentrations <- c(4, 1.6, 0.64, 0.256, 0.1024, 0.041)
    obj@Model.Experiment <- lm(log2(obj@Avg.ExperimentalCt[1:6]) ~ log2(fixed.concentrations))
    obj@Model.Control <- lm(log2(obj@Avg.ControlCt[1:6]) ~ log2(fixed.concentrations))
    ## get predicted concentrations using transformed fit data from models
    obj@Fit.ExperimentalConc <- 2^((log2(obj@Avg.ExperimentalCt) - summary(obj@Model.Experiment)$coeff[1,1]) / summary(obj@Model.Experiment)$coeff[2,1])
    obj@Fit.ControlConc <- 2^((log2(obj@Avg.ExperimentalCt) - summary(obj@Model.Experiment)$coeff[1,1]) / summary(obj@Model.Experiment)$coeff[2,1])
    ## compute T/S ratio as simple division of above
    obj@TS.Ratio <- obj@Fit.ExperimentalConc / obj@Fit.ControlConc
    ## normalization divides the above T/S ratio by the average T/S ratio of internal control samples.
    ## without pasted sample data, I don't believe this is currently available information.
    ## so for the moment, just add placeholder values.
    obj@Normalized.TS <- obj@TS.Ratio
    ## that's it?
    obj
}
