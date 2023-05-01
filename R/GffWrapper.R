#' Wrapper for a GFF file
#'
#' Wrap a GFF2/3 file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a GFF file.
#' @param compression String specifying the compression.
#' This should be one of \code{"none"}, \code{"gzip"}, \code{"bzip2"} or \code{"bgzip"}.
#' If \code{NULL}, this is inferred from the file's headers and suffix.
#' @param index String specifying the path to an index file in tabix format, or \code{NULL} if no index is available.
#' If an index is supplied, the file should be bgzip-compressed.
#' @param format String specifying the format of the GFF file.
#' This should be one of \code{"GFF2"} (i.e., GTF) or \code{"GFF3"}.
#' If \code{NULL}, this is inferred from the file extension of \code{path}.
#'
#' @details
#' The GffWrapper class is a subclass of a \linkS4class{CompressedIndexedWrapper},
#' so all of the methods of the latter can also be used here,
#' e.g., \code{path}, \code{index}, \code{compression}.
#' 
#' @author Aaron Lun
#'
#' @return A GffWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Using rtracklayer's example GFF file.
#' test_path <- system.file("tests", package = "rtracklayer")
#' test_gff3 <- file.path(test_path, "genes.gff3")
#'
#' # Creating a GffWrapper.
#' wrapped <- GffWrapper(test_gff3)
#' wrapped
#'
#' # Staging the GffWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "my_gff")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "my_gff/file.gff3")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' GffWrapper-class
#' stageObject,GffWrapper-method
#' show,GffWrapper-method
#' loadGffWrapper
#' @export
GffWrapper <- function(path, compression=NULL, index=NULL, format=NULL) {
    if (is.null(format)) {
        if (endsWith(path, ".gff2") || endsWith(path, ".gtf")) {
            format <- "GFF2"
        } else if (endsWith(path, ".gff3")) {
            format <- "GFF3"
        } else {
            stop("cannot automatically determine GFF format from file extension")
        }
    }
    construct_compressed_indexed_wrapper(path, compression=compression, index=index, wrapper_class="GffWrapper", index_constructor=TabixWrapper, format=format)
}

#' @export
#' @importFrom alabaster.base .stageObject stageObject .writeMetadata .processMetadata
setMethod("stageObject", "GffWrapper", function(x, dir, path, child=FALSE) {
    ext <- if (x@format == "GFF2") "gff2" else "gff3"
    info <- save_compressed_indexed_wrapper(x, dir, path, fname=paste0("file.", ext), index_class="TabixWrapper")

    meta <- list(
        "$schema" = "gff_file/v1.json",
        path = info$path,
        gff_file = info$inner
    )
    meta$gff_file$format <- x@format

    meta
})

#' @export
setMethod("show", "GffWrapper", function(object) {
    callNextMethod()
    cat("format:", object@format, "\n")
})

#' @export
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
loadGffWrapper <- function(meta, project) {
    load_compressed_indexed_wrapper(meta$path, meta$gff_file, project, constructor=GffWrapper, format=meta$gff_file$format)
}
