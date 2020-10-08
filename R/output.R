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
#' @param top.path character vector of output top-most directory
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
