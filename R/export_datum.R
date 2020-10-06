#' Store aligned telomere and control data per experiment
#'
#' @slot Pos factor of well locations for datapoints on a 384 well plate
#' @slot Cp.Telo numeric vector of cycle threshold data for telomere data
#' @slot Cp.Control numeric vector of cycle threshold data for control gene data
#' @keywords telomeres
#' @seealso [read.export.datum()] for loading data into `ExportDatum`
#' @examples
#' new("ExportDatum", Pos = factor(c("A1", "A2", "A3")), Cp.Telo = runif(3, 0, 38), Cp.Control = runif(3, 0, 38))
#' 
setClass("ExportDatum", slots = list(Pos = "factor", Cp.Telo = "vector", Cp.Control = "vector"))

#' Read and align Cp data for telomeres and controls in an experiment
#'
#' @description
#' `read.export.datum` reads a pair of exported files containing separated telomere and control
#' data and aligns them for later use
#'
#' @details
#' This function removes extraneous metadata and provides mild control checking. It does
#' not postprocess the data in any meaningful way. This function should not be called
#' directly by the end user.
#'
#' @param exp.control.filenames length-2 vector of filenames: one for Telo, one for 36B4
#' @return an instance of S4 class `ExportDatum` containing aligned relevant information.
#' @keywords telomeres
#' @seealso [find.input.files()] for generating the expected input for this function.
#' @examples
#' read.export.datum(c("Data/Exports/PC29625_A_Telo.txt", "Data/Exports/PC29624_A_36B4.txt"))
#' 
read.export.datum <- function(exp.control.filenames) {
	stopifnot(is.vector(exp.control.filenames, mode = "character"))
	stopifnot(length(exp.control.filenames) == 2)
	exp.data <- read.table(exp.control.filenames[1], sep = "\t", skip = 1, header = TRUE)
	control.data <- read.table(exp.control.filenames[2], sep = "\t", skip = 1, header = TRUE)
        stopifnot(identical(exp.data$Pos, control.data$Pos))
	new("ExportDatum", Pos = exp.data$Pos, Cp.Telo = exp.data$Cp, Cp.Control = control.data$Cp)
}
