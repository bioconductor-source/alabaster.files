#' Wrapper for a FASTA index file
#'
#' Wrap a FASTA index file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a FASTA index file.
#'
#' @details
#' The FaIndexWrapper class is a subclass of a \linkS4class{Wrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}.
#' 
#' @author Aaron Lun
#'
#' @return A FaIndexWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a FASTA index file.
#' test_fai <- system.file("extdata", "ce2dict1.fa.fai", package="Rsamtools")
#'
#' # Creating a FaIndexWrapper.
#' wrapped <- FaIndexWrapper(test_fai)
#' wrapped
#'
#' # Staging the FaIndexWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "tab")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "tab/file.fa.fai")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' FaIndexWrapper-class
#' stageObject,FaIndexWrapper-method
#' loadFaIndexWrapper
#' @export
FaIndexWrapper <- function(path) {
    new("FaIndexWrapper", path=path)
}

#' @export
setMethod("stageObject", "FaIndexWrapper", function(x, dir, path, child=FALSE, parent="file.fa", parent.compression="none") {
    info <- save_wrapper(x, dir, path, fname=paste0(parent, ".fai"))
    list(
        "$schema" = "fa_index_file/v1.json",
        path = info$path,
        fa_index_file = info$inner
    )
})

#' @export
loadFaIndexWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$fa_index_file, project, constructor=FaIndexWrapper)
}
