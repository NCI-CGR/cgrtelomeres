#' Store aligned telomere and control data per experiment
#'
#' @slot Analysis.Code character vector describing alphabetic analysis
#' code (A-H)
#' @slot Cp.Telo numeric vector of cycle threshold data for telomere data
#' @slot Cp.Control numeric vector of cycle threshold data for control gene
#' data
#' @slot Standard numeric vector of standard concentration, or NA if not a
#' standard sample
#' @slot Well.ID character vector of well ID codes from Source Plate Contents
#' @slot Vial.ID character vector of vial ID codes from Source Plate Contents
#' @keywords telomeres
#' @seealso [read.export.datum()] for loading data into `ExportDatum`
#' @examples
#' new("ExportDatum",
#'   Cp.Telo = runif(3, 0, 38),
#'   Cp.Control = runif(3, 0, 38),
#'   Standard = rep(NA, 3),
#'   Well.ID = rep(NA, 3),
#'   Vial.ID = rep(NA, 3)
#' )
setClass("ExportDatum", slots = list(
  Analysis.Code = "vector",
  Cp.Telo = "vector",
  Cp.Control = "vector",
  Standard = "vector",
  Well.ID = "vector",
  Vial.ID = "vector"
))

#' Read and align Cp data for telomeres and controls in an experiment
#'
#' @description
#' `read.export.datum` reads a pair of exported files containing separated
#' telomere and control data and aligns them for later use
#'
#' @details
#' This function removes extraneous metadata and provides mild control
#' checking. It does not postprocess the data in any meaningful way.
#' This function should not be called directly by the end user.
#'
#' Note that for workaround purposes while I'm figuring out how the end
#' user is going to specify the evidently external location of the "Source
#' Plate Contents" Data/Analysis pasted spreadsheet tab, I'm just going to
#' pull the source plate contents from the existing spreadsheets. This will
#' be removed at a future date, but allows development and testing to
#' continue for the time being.
#'
#' @param exp.control.filenames Length-2 vector of filenames: one for
#' Telo, one for 36B4
#' @param source.search.path Character vector with path
#' to test case Data/Analysis *.xlsx files
#' @param subject.list.from.input.path Logical: whether to pull subject
#' data from Data/Analysis/*xlsx of an existing analysis run
#' @param plate.content.report Character vector of plate content report
#' for project (e.g. "PlateContentReport_GP0317-TL1.xls") or NA
#' @param plate.list Character vector of plate list for project (e.g.
#' "PlateList_GP0317-TL1.xls") or NA
#' @return An instance of S4 class `ExportDatum` containing aligned
#' relevant information.
#' @keywords telomeres
#' @seealso [find.input.files()] for generating the expected input for
#' this function.
#' @examples
#' read.export.datum(c(
#'   "Data/Exports/PC29625_A_Telo.txt",
#'   "Data/Exports/PC29624_A_36B4.txt"
#' ))
read.export.datum <- function(exp.control.filenames,
                              source.search.path = NA,
                              subject.list.from.input.path = NA,
                              plate.content.report = NA,
                              plate.list = NA) {
  ## assorted input checks
  stopifnot(is.vector(exp.control.filenames, mode = "character"))
  stopifnot(length(exp.control.filenames) == 2)
  stopifnot(is.vector(source.search.path,
    mode = "character"
  ) |
    isTRUE(is.na(source.search.path)))
  stopifnot(is.logical(subject.list.from.input.path) |
    isTRUE(is.na(subject.list.from.input.path)))
  stopifnot(is.vector(plate.content.report, mode = "character") |
    isTRUE(is.na(plate.content.report)))
  stopifnot(is.vector(plate.list, mode = "character") |
    isTRUE(is.na(plate.list)))
  ## read exported data
  exp.data <- read.table(exp.control.filenames[1],
    sep = "\t", skip = 1, header = TRUE
  )
  control.data <- read.table(exp.control.filenames[2],
    sep = "\t", skip = 1, header = TRUE
  )
  stopifnot(identical(exp.data$Pos, control.data$Pos))
  ## set '0' entries to NA, as that's actually the correct interpretation
  control.data$Standard[control.data$Standard < 2e-16] <- NA
  obj <- new("ExportDatum",
    Analysis.Code = gsub(
      "^.*/[A-Z0-9]+_([A-Z])_.*$", "\\1",
      exp.control.filenames[1]
    ),
    Cp.Telo = exp.data$Cp,
    Cp.Control = control.data$Cp,
    Standard = control.data$Standard
  )
  names(obj@Cp.Telo) <- exp.data$Pos
  names(obj@Cp.Control) <- exp.data$Pos
  names(obj@Standard) <- exp.data$Pos

  ## deal with Source Plate Contents
  ## if the caller requests subject list information pulled from an
  ## existing Data/Analysis/*xlsx file
  if (isTRUE(!is.na(subject.list.from.input.path)) &
    subject.list.from.input.path &
    isTRUE(!is.na(source.search.path))) {
    stopifnot(dir.exists(source.search.path))
    target.source.file <- list.files(
      source.search.path,
      paste("^.+_",
        obj@Analysis.Code,
        "\\.xlsx$",
        sep = ""
      )
    )
    stopifnot(is.vector(target.source.file, mode = "character"))
    stopifnot(length(target.source.file) == 1)
    source.plate.contents <-
      openxlsx::read.xlsx(paste(source.search.path,
        target.source.file,
        sep = "/"
      ),
      sheet = 1, rowNames = FALSE, colNames = TRUE
      )
    stopifnot(length(which(colnames(source.plate.contents) ==
      "Well.ID")) == 1)
    stopifnot(length(which(colnames(source.plate.contents) ==
      "Vial.ID")) == 1)
    source.plate.contents[, "Well.ID"] <-
      as.vector(source.plate.contents[, "Well.ID"], mode = "character")
    source.plate.contents[, "Vial.ID"] <-
      as.vector(source.plate.contents[, "Vial.ID"], mode = "character")
    obj@Well.ID <- source.plate.contents[, "Well.ID"]
    obj@Vial.ID <- source.plate.contents[, "Vial.ID"]
  } else if (isTRUE(!is.na(plate.content.report)) &
    isTRUE(!is.na(plate.list))) {
    ## the caller provides enough information to pull data from
    ## a plate content report and list
    plate.content.data <- openxlsx::read.xlsx(plate.content.report,
      sheet = 1,
      rowNames = FALSE,
      colNames = TRUE
    )
    plate.list.data <- openxlsx::read.xlsx(plate.list,
      sheet = 1,
      rowNames = FALSE,
      colNames = TRUE
    )
    ## pending further example datasets, the current link is: content file
    ## column "Description" will have an entry
    ## "{Project.Code}_Intermediate_{Analysis.Code}". That row's
    ## corresponding "Identifier" entry links to the plate content
    ## file column "Plate.ID", and then you want the content file entries
    ## "Well.ID" and "Vial.ID" as above
    stopifnot(length(which(colnames(plate.list.data) ==
      "Description")) == 1)
    stopifnot(length(which(colnames(plate.list.data) ==
      "Identifier")) == 1)
    stopifnot(length(which(colnames(plate.content.data) ==
      "Plate.ID")) == 1)
    stopifnot(length(which(colnames(plate.content.data) ==
      "Well.ID")) == 1)
    stopifnot(length(which(colnames(plate.content.data) ==
      "Vial.ID")) == 1)
    matched.index <- which(grepl(
      paste("^.*_Intermediate_",
        obj@Analysis.Code,
        "$",
        sep = ""
      ),
      plate.list.data$Description
    ))
    stopifnot(length(matched.index) == 1)
    stopifnot(length(which(plate.content.data$Plate.ID ==
      plate.list.data$Identifier[matched.index])) > 0)
    target.contents <-
      plate.content.data[plate.content.data$Plate.ID ==
        plate.list.data$Identifier[matched.index], ]
    target.contents$Well.ID <- as.vector(target.contents$Well.ID,
      mode = "character"
    )
    target.contents <- target.contents[order(target.contents$Well.ID), ]
    obj@Well.ID <- target.contents$Well.ID
    obj@Vial.ID <- target.contents$Vial.ID
  } else {
    stop(paste("In read.export.datum(), no valid method for pulling ",
      "source plate contents detected",
      sep = ""
    ))
  }
  ## end dealing with Source Plate Contents

  obj
}
