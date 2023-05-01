#' Wrapper for a BAM index file
#'
#' Wrap a BAM index file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a BAM index file.
#'
#' @details
#' The BamIndexWrapper class is a subclass of a \linkS4class{Wrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}.
#' 
#' @author Aaron Lun
#'
#' @return A BamIndexWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a BAM index file.
#' test_bai <- system.file("extdata", "ex1.bam.bai", package="Rsamtools")
#'
#' # Creating a BamIndexWrapper.
#' wrapped <- BamIndexWrapper(test_bai)
#' wrapped
#'
#' # Staging the BamIndexWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "tab")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "tab/file.bai")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' BamIndexWrapper-class
#' stageObject,BamIndexWrapper-method
#' loadBamIndexWrapper
#' @export
BamIndexWrapper <- function(path) {
    new("BamIndexWrapper", path=path)
}

#' @export
setMethod("stageObject", "BamIndexWrapper", function(x, dir, path, child=FALSE, parent="file") {
    info <- save_wrapper(x, dir, path, fname=paste0(parent, ".bai"))
    list(
        "$schema" = "bam_index_file/v1.json",
        path = info$path,
        bam_index_file = info$inner
    )
})

#' @export
loadBamIndexWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$bam_index_file, project, constructor=BamIndexWrapper)
}
