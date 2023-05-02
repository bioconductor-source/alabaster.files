#' Wrapper for a BAM file
#'
#' Wrap a BAM file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a BAM file.
#' @param index String specifying the path to an index file in tabix format, or \code{NULL} if no index is available.
#'
#' @details
#' The BamWrapper class is a subclass of a \linkS4class{IndexedWrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}, \code{index}.
#' 
#' The \code{stageObject} method for BamWrapper classes will check the BAM file by scanning the header with \code{\link{scanBamHeader}}.
#' 
#' @author Aaron Lun
#'
#' @return A BamWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Using a BAM file from Rsamtools.
#' fl <- system.file("extdata", "ex1.bam", package="Rsamtools", mustWork=TRUE)
#'
#' # Creating a BamWrapper.
#' wrapped <- BamWrapper(fl)
#' wrapped
#'
#' # Staging the BamWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "my_bam")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "my_bam/file.bam")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' BamWrapper-class
#' stageObject,BamWrapper-method
#' loadBamWrapper
#' @export
BamWrapper <- function(path, index=NULL) {
    construct_indexed_wrapper(path, index=index, wrapper_class="BamWrapper", index_constructor=BamIndexWrapper)
}

#' @export
#' @importFrom Rsamtools scanBamHeader
setMethod("stageObject", "BamWrapper", function(x, dir, path, child=FALSE) {
    scanBamHeader(x@path) # validating it by scanning the header.

    info <- save_indexed_wrapper(x, dir, path, fname="file.bam", index_class="BamIndexWrapper")
    list(
        "$schema" = "bam_file/v1.json",
        path = info$path,
        bam_file = info$inner
    )
})

#' @export
loadBamWrapper <- function(meta, project) {
    load_indexed_wrapper(meta$path, meta$bam_file, project, constructor=BamWrapper)
}
