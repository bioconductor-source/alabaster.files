#' Wrapper for a FASTA file
#'
#' Wrap a FASTA file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a FASTA file.
#' @param compression String specifying the compression.
#' This should be one of \code{"none"}, \code{"gzip"}, \code{"bzip2"} or \code{"bgzip"}.
#' If \code{NULL}, this is inferred from the file's headers and suffix.
#' @param index String specifying the path to an faidx file, or \code{NULL} if no index is available.
#' If an index is supplied, the file should be uncompressed or bgzip-compressed.
#'
#' @details
#' The FastaWrapper class is a subclass of a \linkS4class{CompressedIndexedWrapper},
#' so all of the methods of the latter can also be used here,
#' e.g., \code{path}, \code{index}, \code{compression}.
#' 
#' @author Aaron Lun
#'
#' @return A FastaWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a FASTA file.
#' tmp <- file.path(fileext=".fa")
#' write(">FOOBAR\nacgtacgt", tmp)
#'
#' # Creating a FastaWrapper.
#' wrapped <- FastaWrapper(tmp)
#' wrapped
#'
#' # Staging the FastaWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "seq")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "seq/file.fa")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' FastaWrapper-class
#' stageObject,FastaWrapper-method
#' loadFastaWrapper
#' @export
FastaWrapper <- function(path, sequence.type="DNA", compression=NULL, index=NULL) {
    construct_compressed_indexed_wrapper(
        path, 
        compression=compression, 
        index=index, 
        wrapper_class="FastaWrapper", 
        index_constructor=FastaIndexWrapper,
        sequence.type=sequence.type
    )
}

#' @export
#' @importFrom alabaster.base .stageObject stageObject .writeMetadata .processMetadata
setMethod("stageObject", "FastaWrapper", function(x, dir, path, child=FALSE) {
    info <- save_compressed_indexed_wrapper(x, dir, path, fname="file.fa", index_class="FaidxWrapper", type=x@sequence.type)
    list(
        "$schema" = "fasta_file/v1.json",
        path = info$path,
        fasta_file = info$inner
    )
})

setMethod("showheader", "FastaWrapper", function(object) {
    cat(class(object)[1], "object containing", object@sequence.type, "sequences\n")
})

#' @export
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
loadFastaWrapper <- function(meta, project) {
    load_compressed_indexed_wrapper(
        meta$path, 
        meta$fasta_file, 
        project, 
        constructor=FastaWrapper,
        sequence.type=meta$fasta_file$type
    )
}
