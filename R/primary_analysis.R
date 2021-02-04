#' Store primary analysis results, processed from ExportDatum
#'
#' @slot Analysis.Code character vector describing alphabetic
#' analysis code A-H
#' @slot Source.Well.ID factor of well IDs from source data
#' @slot Internal.Control boolean vector of whether a sample is an
#' internal control
#' @slot Source.Plate.ID character vector of Intermediate Source Plate ID
#' @slot Well.ID character vector of source plate well ID
#' @slot Sample.ID character vector of sample ID
#' @slot Vial.ID character vector of vial ID
#' @slot Rep1.Well factor of 384 well plate locations for each first replicate
#' @slot Rep2.Well factor of 384 well plate locations for each second replicate
#' @slot Rep3.Well factor of 384 well plate locations for each third replicate
#' @slot Rep1.ExperimentalCt.prefilter numeric vector of raw telomeric
#' Ct values for each first replicate
#' @slot Rep2.ExperimentalCt.prefilter numeric vector of raw telomeric
#' Ct values for each second replicate
#' @slot Rep3.ExperimentalCt.prefilter numeric vector of raw telomeric
#' Ct values for each third replicate
#' @slot Rep1.ControlCt.prefilter numeric vector of raw control Ct
#' values for each first replicate
#' @slot Rep2.ControlCt.prefilter numeric vector of raw control Ct
#' values for each second replicate
#' @slot Rep3.ControlCt.prefilter numeric vector of raw control Ct
#' values for each third replicate
#' @slot Rep1.ExperimentalCt.postfilter numeric vector of post-filtering
#' telomeric Ct values for each first replicate
#' @slot Rep2.ExperimentalCt.postfilter numeric vector of post-filtering
#' telomeric Ct values for each second replicate
#' @slot Rep3.ExperimentalCt.postfilter numeric vector of post-filtering
#' telomeric Ct values for each third replicate
#' @slot Rep1.ControlCt.postfilter numeric vector of post-filtering
#' control Ct values for each first replicate
#' @slot Rep2.ControlCt.postfilter numeric vector of post-filtering
#' control Ct values for each second replicate
#' @slot Rep3.ControlCt.postfilter numeric vector of post-filtering
#' control Ct values for each third replicate
#' @slot Avg.ExperimentalCt numeric vector of mean post-filtering
#' telomeric Ct values
#' @slot SD.ExperimentalCt numeric vector of standard deviation of
#' post-filtering telomeric Ct values
#' @slot PerCV.ExperimentalCt numeric vector of
#' post-filtering telomeric Ct values
#' @slot Avg.ControlCt numeric vector of mean post-filtering control Ct values
#' @slot SD.ControlCt numeric vector of standard deviation of post-filtering
#' control Ct values
#' @slot PerCV.ControlCt numeric vector of post-filtering
#' control Ct values
#' @slot Model.Experiment object of class lm, fit model for telomeric data
#' @slot Model.Control object of class lm, fit model for control data
#' @slot Fit.ExperimentalConc numeric vector of exponential fit values
#' from standard curve for telomeric data
#' @slot Fit.ControlConc numeric vector of exponential fit values from
#' standard curve for control data
#' @slot TS.Ratio numeric vector of ratio of telomeric to control fit
#' @slot Normalized.TS numeric vector of TS ratio normalized to controls
#' @keywords telomeres
#' @examples
#' new("PrimaryAnalysis")
setClass("PrimaryAnalysis", slots = list(
  Analysis.Code = "vector",
  Source.Well.ID = "factor",
  Internal.Control = "vector",
  Source.Plate.ID = "vector",
  Well.ID = "vector",
  Sample.ID = "vector",
  Vial.ID = "vector",
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
  Normalized.TS = "vector"
))

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
PrimaryAnalysis <- function(...) {
  obj <- new("PrimaryAnalysis", ...)
  shared.levels <- paste(rep(LETTERS[1:16], each = 24),
    rep(1:24, 16),
    sep = ""
  )
  obj@Rep1.Well <- factor(c(
    paste(LETTERS[seq(2, by = 2, length.out = 7)],
      rep(2, 7),
      sep = ""
    ),
    paste(LETTERS[seq(1, by = 2, length.out = 8)],
      rep(seq(1, 23, 2), each = 8),
      sep = ""
    )
  ),
  levels = shared.levels
  )
  obj@Rep2.Well <- factor(c(
    paste(LETTERS[seq(2, by = 2, length.out = 7)],
      rep(4, 7),
      sep = ""
    ),
    paste(LETTERS[seq(1, by = 2, length.out = 8)],
      rep(seq(2, 24, 2), each = 8),
      sep = ""
    )
  ),
  levels = shared.levels
  )
  obj@Rep3.Well <- factor(c(
    paste(LETTERS[seq(2, by = 2, length.out = 7)],
      rep(6, 7),
      sep = ""
    ),
    paste(LETTERS[seq(2, by = 2, length.out = 8)],
      rep(seq(1, 23, 2), each = 8),
      sep = ""
    )
  ),
  levels = shared.levels
  )
  obj@Source.Well.ID <- factor(c(
    rep("Standards", 6),
    "NTC",
    paste(rep(LETTERS[1:8], 12),
      sprintf("%02d", rep(1:12, each = 8)),
      sep = "-"
    )
  ))
  obj@Internal.Control <- rep(
    0,
    length(obj@Source.Well.ID)
  )
  obj
}

#' Apply Ct filtering to PrimaryAnalysis intermediates based on some criteria
#'
#' @description
#' `apply.Ct.filtering` takes raw Ct(Cp) values and applies mild postprocessing
#' to conform with current behavior of the analysis protocol.
#'
#' @details
#' The current filter applied is "Ct equal to 38 is set to NA". This is
#' different than what's applied in the excel template (equal to 28,
#' set to 0), but based on how the Cts are defined, the 28 criterion
#' is actually a bug in the template. By inspection, it seems like the
#' actual current protocol is to go through and manually blank entries
#' that exceed some Ct threshold, possibly closer to 30. This function
#' provides centralized control should that filter need to be adjusted
#' in the future.
#'
#' @param vec Numeric vector of raw Ct values.
#' @return Numeric vector of input with some overlarge values set to NA.
#' @keywords telomeres
#' @seealso [read.export.datum()] for acquiring raw Ct values,
#' [PrimaryAnalysis()]
#' for construction of PrimaryAnalysis objects that use these processed Cts.
#' @examples
#' filtered.data <- apply.Ct.filtering(raw.data)
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
#' real data example, where conditions with fewer than two replicates are
#' blanked. Hopefully, this should cover that situation, while allowing
#' single dropouts.
#'
#' @param vec.rep1 numeric vector of first replicate Ct data
#' @param vec.rep2 numeric vector of second replicate Ct data
#' @param vec.rep3 numeric vector of third replicate Ct data
#' @return numeric vector of vector-wise averages, with NA
#' exclusion criteria applied
#' @seealso [process.standard.deviation()] for equivalent calculation
#' with standard deviation.
#' @examples
#' vec.mean <- process.average(rnorm(10), rnorm(10), rnorm(10))
process.average <- function(vec.rep1,
                            vec.rep2,
                            vec.rep3) {
  apply(
    cbind(vec.rep1, vec.rep2, vec.rep3),
    1,
    function(i) {
      ifelse(length(which(is.na(i))) < 2, mean(i, na.rm = TRUE), NA)
    }
  )
}

#' Compute replicate standard deviation with restrictions
#'
#' @description
#' `process.standard.deviation` takes three replicates' data and
#' combines vector-wise into standard deviations while allowing a
#' single observation from each condition at most to be missing
#' (flagged as NA).
#'
#' @details
#' There is an implicit restriction that appears to be applied manually to the
#' real data example, where conditions with fewer than two replicates are
#' blanked. Hopefully, this should cover that situation, while allowing
#' single dropouts. Strangely, the existing excel template uses sample
#' standard deviation for telomere data and population standard deviation
#' for control data. I don't know why this is the case but it's emulated
#' here for the moment.
#'
#' @param vec.rep1 numeric vector of first replicate Ct data
#' @param vec.rep2 numeric vector of second replicate Ct data
#' @param vec.rep3 numeric vector of third replicate Ct data
#' @param use.sample.adjustment boolean of whether to use sqrt(N - 1)
#' biased sample adjustment
#' @return numeric vector of vector-wise averages, with NA exclusion
#' criteria applied
#' @seealso [process.average()] for equivalent calculation with mean
#' @examples
#' vec.sd <- process.standard.deviation(rnorm(10), rnorm(10), rnorm(10))
process.standard.deviation <- function(vec.rep1,
                                       vec.rep2,
                                       vec.rep3,
                                       use.sample.adjustment = TRUE) {
  apply(
    cbind(vec.rep1, vec.rep2, vec.rep3),
    1,
    function(i) {
      ifelse(length(which(is.na(i))) < 2,
        sd(i, na.rm = TRUE) *
          ifelse(use.sample.adjustment,
            1,
            sqrt((length(which(!is.na(i))) - 1) /
              length(which(!is.na(i))))
          ), NA
      )
    }
  )
}

#' Provided a standard curve, predict concentration from Ct
#'
#' @description
#' `compute.fit` takes a fit transformed exponential regression model and uses
#' its coefficients to predict best fit values for provided Cts, given
#' boundaries on valid estimates defined by the standards.
#'
#' @details
#' The transformation applied to the data assumes that regression has
#' been performed on log2-transformed predictors (concentrations) and
#' outcomes (Cts).
#'
#' @param fit.model Object of class `lm` from which regression
#' coefficients are extracted.
#' @param cycle.thresholds Numeric vector of Ct values to convert to
#' best fit concentrations.
#' @param standard.limits Numeric vector of length 2, presumably the
#' output of `range`
#' @return Numeric vector of best fit concentrations, with out of bounds
#' values set to NA.
#' @keywords telomeres
#' @seealso [create.analysis()] for the standard curve calculation;
#' [lm()] for linear regression.
#' @examples
#' fixed.concentrations <- c(4, 1.6, 0.64, 0.256, 0.1024, 0.041)
#' experimental.cts <- runif(20, 1, 38)
#' fit.model <- lm(log2(experimental.cts[1:6]) ~ log2(fixed.concentrations))
#' fit.values <- compute.fit(fit.model, experimental.cts, fixed.concentrations)
compute.fit <- function(fit.model,
                        predictors,
                        standard.limits) {
  stopifnot(is(fit.model, "lm"))
  stopifnot(is.vector(predictors, mode = "numeric"))
  stopifnot(is.vector(standard.limits, mode = "numeric"))
  stopifnot(length(standard.limits) == 2)
  stopifnot(standard.limits[1] <= standard.limits[2])
  res.exponent <- (log2(predictors) - summary(fit.model)$coeff[1, 1]) /
    summary(fit.model)$coeff[2, 1]
  res <- 2^res.exponent
  res[predictors < standard.limits[1] | predictors > standard.limits[2]] <- NA
  res
}


#' Map 96 to 384 wells using Plate List and Content Report data
#'
#' @description
#' `extract.well.mappings` takes plate list and content data and extracts
#' the actual 96->384 well mapping used in the analysis, as opposed to
#' the default one observed in the initial test case.
#'
#' @details
#' The output is sorted lexicographically by 96 well label. Replicate
#' assignments (1 vs 2 vs 3) are similarly resolved by lexicographical
#' sort of the 384 well labels. This should compel the output to exactly
#' match the previous protocol's Data/Analysis/*xlsx format if used
#' without further shuffling.
#'
#' @param plate.list.data Data frame of plate list data
#' @param plate.content.data Data frame of plate content report data
#' @param analysis.code Character vector: case-sensitive letter A-H
#' for run code
#' @return List with entries Rep1.Well, Rep2.Well, Rep3.Well, corresponding to
#' columns in the Data/Analysis/*xlsx files. Each will be a factor.
#' @keywords telomeres
#' @seealso [create.analysis()] for calling function;
#' [openxlsx::read.xlsx()] for an example of how to generate the plate
#' list or content data frames.
#' @examples
#' content.filename <- "PlateContentReport_GP0317-TL1.xls"
#' list.filename <- "PlateList_GP0317-TL1.xls"
#' content.data <- openxlsx:read.xlsx(content.filename,
#'   sheet = 1,
#'   rowNames = FALSE, colNames = TRUE
#' )
#' list.data <- openxlsx::read.xlsx(list.filename,
#'   sheet = 1,
#'   rowNames = FALSE, colNames = TRUE
#' )
#' analysis.code <- "H"
#' well.mappings <- extract.well.mappings(
#'   list.data, content.data,
#'   analysis.code
#' )
extract.well.mappings <- function(plate.list.data,
                                  plate.content.data,
                                  analysis.code) {
  ## input format checking
  stopifnot(is.data.frame(plate.list.data))
  stopifnot(length(which(colnames(plate.list.data) == "Description")) == 1)
  stopifnot(length(which(colnames(plate.list.data) == "Identifier")) == 1)
  stopifnot(is.data.frame(plate.content.data))
  stopifnot(length(which(colnames(plate.content.data) == "Well.ID")) == 1)
  stopifnot(length(which(colnames(plate.content.data) == "Vial.ID")) == 1)
  stopifnot(length(which(colnames(plate.content.data) == "Plate.ID")) == 1)
  stopifnot(length(which(colnames(plate.content.data) == "2D.Barcode")) == 1)
  stopifnot(is.vector(analysis.code, mode = "character"))
  ## process for this is as follows:
  ## 1) in plate list, get Identifier column entry corresponding to
  ##    Description "{project.id}/Telo {obj@Analysis.Code}"
  ## 2) pull the plate content Well.ID and Vial.ID entries for rows
  ##    in Plate.ID matching that list Description
  ## 3) map replicate wells from Well.ID, parsing out various things
  matched.exp.plate.index <- which(grepl(
    paste(
      "^.*/Telo ",
      analysis.code,
      "$",
      sep = ""
    ),
    plate.list.data$Description
  ))
  matched.control.plate.index <- which(grepl(
    paste("^.*/36B4 ",
      analysis.code,
      "$",
      sep = ""
    ),
    plate.list.data$Description
  ))
  stopifnot(length(matched.exp.plate.index) == 1)
  stopifnot(length(matched.control.plate.index) == 1)
  experimental.plate.id <-
    plate.list.data$Identifier[
      matched.exp.plate.index
    ]
  control.plate.id <- plate.list.data$Identifier[matched.control.plate.index]
  matched.experimental.content <-
    plate.content.data[plate.content.data$Plate.ID ==
      experimental.plate.id, ]
  matched.control.content <-
    plate.content.data[plate.content.data$Plate.ID == control.plate.id, ]
  stopifnot(nrow(matched.experimental.content) > 0)
  stopifnot(nrow(matched.experimental.content) ==
    nrow(matched.control.content))
  stopifnot(identical(
    matched.experimental.content$Vial.ID,
    matched.control.content$Vial.ID
  ))
  ## for what can only be described as Reasons, the 384 well labels
  ## do not have leading "0"s prefixed to Data/Analysis/*xlsx columns
  ## D-F labels. so those need to be parsed away if present
  parsed.well.ids <- paste(gsub(
    "^([A-Z])-[0-9]+$", "\\1",
    matched.experimental.content$Well.ID
  ),
  as.vector
  (gsub(
      "^[A-Z]-([0-9]+)$", "\\1",
      matched.experimental.content$Well.ID
    ),
    mode = "numeric"
  ),
  sep = ""
  )
  matched.experimental.content$parsed.well.ids <- parsed.well.ids
  ## need to deal with controls
  ## the existing analysis flags six (each with three replicates)
  ## standards of known concentration; a fixed NTC (with three replicates);
  ## some number of additional triplicate NTCs within the samples;
  ## and a number of triplicate "Internal Control" subjects within the
  ## samples. how do we figure out which is which, and how is each flagged?
  ## Standards:
  ##    How to detect: empty "2D.Barcode" content entry; "Vial.ID" content
  ##    entry is pure numeric in {4,1.6,0.64,0.256,0.0124,0.04096}
  ##    How to handle: for each replicate, report as the first six entries
  ##    in order of descending concentration
  ## NTC:
  ##    How to detect: null entries for both "2D.Barcode" and "Vial.ID"
  ##    How to handle: unclear. the Data/Analysis files always seem to
  ##    list the {N2,N4,N6} NTC as a fixed value immediately after the
  ##    standards, but then the data (if present) are not used for
  ##    anything in particular. I don't know if there's anything
  ##    intrinsically different about the {N2,N4,N6} NTC versus the others.
  ##    perhaps the N# triplicate is the only one guaranteed to be measured?
  ##    possibly need to cross reference with the export txt files. in
  ##    any case, it may be necessary to move all the NTCs to their
  ##    own cluster of results, which honestly isn't a bad thing really:
  ##    presumably someone checking the NTCs wants to check them all at once?
  ##    I think there's also an issue of empty wells that aren't being
  ##    measured at all, and that need to be filtered out before reporting,
  ##    that are still somehow
  ## Internal Controls:
  ##    How to detect: null "2D.Barcode"; "^NA[0-9]+$" "Vial.ID"
  ##    How to handle: there is a vector in ExportDatum that theoretically
  ##                   was going to track internal control status.
  ##                   however, it's not 100% clear to me if that's correct
  ##                   at this point, seeing as the information is more
  ##                   clearly available here.
}


#' Create a PrimaryAnalysis from a loaded ExportDatum
#'
#' @description
#' `create.analysis` takes an ExportDatum object containing telomeric
#' and control data and applies the standard analysis currently being run
#' by the excel template that generates files in Data/Analysis/*xlsx.
#'
#' @details
#' This function attempts to emulate the results in the primary analysis
#' spreadsheet. There's a fair amount of redundant information (log2 transform
#' of some data, linear regression information that is deprecated) that
#' is not being included for the moment.
#'
#' @param export.datum An ExportDatum object with raw qPCR data for processing
#' @param plate.content.report Character vector of plate content report for
#' project (e.g. "PlateContentReport_GP0317-TL1.xls") or NA
#' @param plate.list Character vector of plate list for project (e.g.
#' "PlateList_GP0317-TL1.xls") or NA
#' @param infer.384.locations Logical: whether to assume fixed 96->384
#' well mapping
#' @param control.vials Character vector of internal control vial IDs
#' @return a PrimaryAnalysis object containing the processed results
#' for the input
#' @keywords telomeres
#' @seealso [PrimaryAnalysis()] for constructing empty PrimaryAnalysis objects;
#' [read.export.datum()] for generating compatible input data structures.
#' @examples
#' filename.pair <- find.input.files("Examples for Bioinformatics")
#' export.datum <- read.export.datum(filename.pair)
#' processed.analysis <- create.analysis(export.datum)
create.analysis <- function(export.datum,
                            plate.content.report = NA,
                            plate.list = NA,
                            infer.384.locations = FALSE,
                            control.vials = c("NA07057")) {
  ## basic input error checking: redundant with process.experiment
  ## but good practice regardless
  stopifnot((is.vector(plate.content.report, mode = "character") &
    file.exists(plate.content.report)) |
    isTRUE(is.na(plate.content.report)))
  stopifnot((is.vector(plate.list, mode = "character") &
    file.exists(plate.list)) |
    isTRUE(is.na(plate.list)))
  stopifnot(is.logical(infer.384.locations))
  ## `infer.384.locations` only works if the plate list and
  ## content report are both specified
  stopifnot(!infer.384.locations |
    (infer.384.locations &
      !is.na(plate.content.report) &
      !is.na(plate.list)))
  ## instantiate
  obj <- PrimaryAnalysis()
  ## store the analysis code, a letter from A-H, for output file conventions
  obj@Analysis.Code <- export.datum@Analysis.Code
  obj@Internal.Control[export.datum@Vial.ID %in% control.vials] <- 1


  ## if infer.384.locations, map 96->384 well plates from plate content
  ## report and list in order to recompute names(obj@Rep[1-3].Well)
  if (infer.384.locations & isTRUE(!is.na(plate.list)) &
    isTRUE(!is.na(plate.content.report))) {
    plate.list.data <- openxlsx::read.xlsx(plate.list,
      sheet = 1,
      rowNames = FALSE,
      colNames = TRUE
    )
    plate.content.data <- openxlsx::read.xlsx(plate.content.report,
      sheet = 1,
      rowNames = FALSE,
      colNames = TRUE
    )
    all.replicate.wells <- extract.well.mappings(
      plate.list.data,
      plate.content.data,
      obj@Analysis.code
    )
    shared.levels <- paste(rep(LETTERS[1:16], each = 24),
      rep(1:24, 16),
      sep = ""
    )
    obj@Rep1.Well <- factor(all.replicate.wells[["Rep1.Well"]],
      levels = shared.levels
    )
    obj@Rep2.Well <- factor(all.replicate.wells[["Rep2.Well"]],
      levels = shared.levels
    )
    obj@Rep3.Well <- factor(all.replicate.wells[["Rep3.Well"]],
      levels = shared.levels
    )
  } else if (infer.384.locations) {
    stop(paste("In create.analysis(): cannot proceed with inferring 384 ",
      "well locations without valid plate list and content report",
      sep = ""
    ))
  }
  ## implied remaining condition is use predicted well assignments,
  ## which is constructed by default

  ## additional fields required for final report
  obj@Source.Plate.ID <- export.datum@Source.Plate.ID
  obj@Well.ID <- export.datum@Well.ID
  obj@Sample.ID <- export.datum@Sample.ID
  obj@Vial.ID <- export.datum@Vial.ID


  ## load Cp data from export datum at the appropriate wells
  obj@Rep1.ExperimentalCt.prefilter <- export.datum@Cp.Telo[obj@Rep1.Well]
  obj@Rep2.ExperimentalCt.prefilter <- export.datum@Cp.Telo[obj@Rep2.Well]
  obj@Rep3.ExperimentalCt.prefilter <- export.datum@Cp.Telo[obj@Rep3.Well]
  obj@Rep1.ControlCt.prefilter <- export.datum@Cp.Control[obj@Rep1.Well]
  obj@Rep2.ControlCt.prefilter <- export.datum@Cp.Control[obj@Rep2.Well]
  obj@Rep3.ControlCt.prefilter <- export.datum@Cp.Control[obj@Rep3.Well]
  ## apply filtering based on *some criterion*
  obj@Rep1.ExperimentalCt.postfilter <-
    apply.Ct.filtering(obj@Rep1.ExperimentalCt.prefilter)
  obj@Rep2.ExperimentalCt.postfilter <-
    apply.Ct.filtering(obj@Rep2.ExperimentalCt.prefilter)
  obj@Rep3.ExperimentalCt.postfilter <-
    apply.Ct.filtering(obj@Rep3.ExperimentalCt.prefilter)
  obj@Rep1.ControlCt.postfilter <-
    apply.Ct.filtering(obj@Rep1.ControlCt.prefilter)
  obj@Rep2.ControlCt.postfilter <-
    apply.Ct.filtering(obj@Rep2.ControlCt.prefilter)
  obj@Rep3.ControlCt.postfilter <-
    apply.Ct.filtering(obj@Rep3.ControlCt.prefilter)
  ## compute average, SD, percent coefficient of variation for each condition.
  ## just by inspection, it seems like conditions with fewer than two
  ## successful replicates are blanked out.
  obj@Avg.ExperimentalCt <- process.average(
    obj@Rep1.ExperimentalCt.postfilter,
    obj@Rep2.ExperimentalCt.postfilter,
    obj@Rep3.ExperimentalCt.postfilter
  )
  obj@SD.ExperimentalCt <- process.standard.deviation(
    obj@Rep1.ExperimentalCt.postfilter,
    obj@Rep2.ExperimentalCt.postfilter,
    obj@Rep3.ExperimentalCt.postfilter
  )
  obj@PerCV.ExperimentalCt <- 100 * obj@SD.ExperimentalCt /
    obj@Avg.ExperimentalCt
  obj@Avg.ControlCt <- process.average(
    obj@Rep1.ControlCt.postfilter,
    obj@Rep2.ControlCt.postfilter,
    obj@Rep3.ControlCt.postfilter
  )
  obj@SD.ControlCt <- process.standard.deviation(
    obj@Rep1.ControlCt.postfilter,
    obj@Rep2.ControlCt.postfilter,
    obj@Rep3.ControlCt.postfilter,
    FALSE
  )
  obj@PerCV.ControlCt <- 100 * obj@SD.ControlCt / obj@Avg.ControlCt
  ## apply coefficient of variation filtering
  ## evidently they use a cutoff of >2 to remove bad replicates
  CV.threshold <- 2
  CV.exclude <- obj@PerCV.ControlCt > CV.threshold |
    obj@PerCV.ExperimentalCt > CV.threshold
  obj@Avg.ExperimentalCt[CV.exclude] <- NA
  obj@Avg.ControlCt[CV.exclude] <- NA
  ## must run exponential regressions against the standard lanes.
  ## now, it *appears* that the standard concentrations are predefined
  ## in the excel template, and are likely fixed across experiments.
  ## set these here for now; possibly expose them later. note that this
  ## model requires post hoc transformation before you get the exact
  ## coefficients that were being produced with excel's `logest`
  fixed.concs <- c(4, 1.6, 0.64, 0.256, 0.1024, 0.041) # nolint
  obj@Model.Experiment <-
    lm(log2(obj@Avg.ExperimentalCt[1:6]) ~ log2(fixed.concs))
  obj@Model.Control <-
    lm(log2(obj@Avg.ControlCt[1:6]) ~ log2(fixed.concs))
  ## get predicted concentrations using transformed fit data from models
  obj@Fit.ExperimentalConc <- compute.fit(
    obj@Model.Experiment,
    obj@Avg.ExperimentalCt,
    range(obj@Avg.ExperimentalCt[1:6], na.rm = TRUE)
  )
  obj@Fit.ControlConc <- compute.fit(
    obj@Model.Control,
    obj@Avg.ControlCt,
    range(obj@Avg.ControlCt[1:6], na.rm = TRUE)
  )
  ## compute T/S ratio as simple division of above
  obj@TS.Ratio <- obj@Fit.ExperimentalConc / obj@Fit.ControlConc
  ## normalization divides the above T/S ratio by the average T/S ratio
  ## of internal control samples. without pasted sample data, I don't
  ## believe this is currently available information.
  ## so for the moment, just add placeholder values.
  obj@Normalized.TS <- obj@TS.Ratio /
    mean(obj@TS.Ratio[obj@Internal.Control == 1], na.rm = TRUE)
  ## that's it?
  obj
}
