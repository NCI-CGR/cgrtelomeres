#' Process a CGR-style telomere qPCR experiment
#'
#' @description
#' `process.experiment` takes the output of a lab telomere qPCR experiment
#' and generates the expected output content from prior processing methods.
#' 
#' @details
#' This is the main entry point for the CGR telomere processing package.
#' The input directory is sanity checked for its expected contents. The output
#' directory will be created if it doesn't already exist. Any existing conflicting
#' output files will be overwritten by this method, so please change output.path
#' if you don't want this behavior.
#' 
#' @param input.path Character vector of input directory path
#' @param output.path Character vector of output directory path
#' @param project.id Character vector of project ID (e.g. "GP0317-TL7")
#' @keywords telomeres
#' @export
#' @examples
#' process.experiment("Examples for Bioinformatics", "C:/Users/palmercd/Documents/telomeres/output", "GP0317-TL7")
#'
process.experiment <- function(input.path, output.path, project.id) {
    ## check parameter requirements
    stopifnot(is.vector(input.path, mode = "character"))
    stopifnot(is.vector(output.path, mode = "character"))
    stopifnot(is.vector(project.id, mode = "character"))
    ## check input directory path exists
    stopifnot(dir.exists(input.path))
    ## if output directory path does not exist, create it
    if (!dir.exists(output.path)) {
        dir.create(output.path)
    }
    ## aggregate pairs of filenames from input.path/Data/Exports
    input.files <- find.input.files(paste(input.path, "Data", "Exports", sep="/"))
    ## load data from acquired pairs of files
    input.data <- lapply(input.files, read.export.datum)
    ## TODO: downstream steps
    input.data
}

