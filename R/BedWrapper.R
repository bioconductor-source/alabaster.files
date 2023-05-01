#' Wrapper for a BED file
#'
#' Wrap a BED file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a BED file.
#' @param compression String specifying the compression.
#' This should be one of \code{"none"}, \code{"gzip"}, \code{"bzip2"} or \code{"bgzip"}.
#' If \code{NULL}, this is inferred from the file's headers and suffix.
#' @param index String specifying the path to an index file in tabix format, or \code{NULL} if no index is available.
#' If an index is supplied, the file should be bgzip-compressed.
#'
#' @details
#' The BedWrapper class is a subclass of a \linkS4class{CompressedIndexedWrapper},
#' so all of the methods of the latter can also be used here,
#' e.g., \code{path}, \code{index}, \code{compression}.
#' 
#' @author Aaron Lun
#'
#' @return A BedWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a BED file.
#' tmp <- tempfile(fileext=".bed")
#' bed <- write("chr1\t2222\t33333", file=tmp)
#'
#' # Creating a BedWrapper.
#' wrapped <- BedWrapper(tmp)
#' wrapped
#'
#' # Staging the BedWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "my_bed")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "my_bed/file.bed")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' BedWrapper-class
#' stageObject,BedWrapper-method
#' loadBedWrapper
#' @export
BedWrapper <- function(path, compression=NULL, index=NULL) {
    construct_compressed_indexed_wrapper(path, compression=compression, index=index, wrapper_class="BedWrapper", index_constructor=TabixWrapper)
}

#' @export
#' @importFrom alabaster.base .stageObject stageObject .writeMetadata .processMetadata
setMethod("stageObject", "BedWrapper", function(x, dir, path, child=FALSE) {
    info <- save_compressed_indexed_wrapper(x, dir, path, fname="file.bed", index_class="TabixWrapper")
    meta <- list(
        "$schema" = "bed_file/v1.json",
        path = info$path,
        bed_file = info$inner
    )

    header <- readLines(x@path, n = 1L)
    format <- 'BED'
    if (length(header)) {
        if (length(strsplit(header, "\t")[[1]]) == 15) {
            format <- 'BED15'
        }
    }
    meta$bed_file$format <- format

    meta
})

#' @export
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
loadBedWrapper <- function(meta, project) {
    load_compressed_indexed_wrapper(meta$path, meta$bed_file, project, constructor=BedWrapper)
}
