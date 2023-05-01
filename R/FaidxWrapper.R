#' Wrapper for a FASTA index file
#'
#' Wrap a FASTA index file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a FASTA index file.
#'
#' @details
#' The FaidxWrapper class is a subclass of a \linkS4class{Wrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}.
#' 
#' @author Aaron Lun
#'
#' @return A FaidxWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a FASTA index file.
#' test_fai <- system.file("extdata", "ce2dict1.fa.fai", package="Rsamtools")
#'
#' # Creating a FaidxWrapper.
#' wrapped <- FaidxWrapper(test_fai)
#' wrapped
#'
#' # Staging the FaidxWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "tab")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "tab/file.fai")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' FaidxWrapper-class
#' stageObject,FaidxWrapper-method
#' loadFaidxWrapper
#' @export
FaidxWrapper <- function(path) {
    new("FaidxWrapper", path=path)
}

#' @export
setMethod("stageObject", "FaidxWrapper", function(x, dir, path, child=FALSE, parent="file") {
    info <- save_wrapper(x, dir, path, fname=paste0(parent, ".fai"))
    list(
        "$schema" = "faidx_file/v1.json",
        path = info$path,
        faidx_file = info$inner
    )
})

#' @export
loadFaidxWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$faidx_file, project, constructor=FaidxWrapper)
}
