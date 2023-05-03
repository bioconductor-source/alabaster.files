#' Wrapper for a FASTA file
#'
#' Wrap a FASTA file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a FASTA file.
#' @param compression String specifying the compression.
#' This should be one of \code{"none"}, \code{"gzip"}, \code{"bzip2"} or \code{"bgzip"}.
#' If \code{NULL}, this is inferred from the file's headers and suffix.
#' @param index String specifying the path to an FASTA index file, or \code{NULL} if no index is available.
#' If an index is supplied, the file at \code{path} should be uncompressed or bgzip-compressed.
#' @param gzindex String specifying the path to a bgzip index file, or \code{NULL} if no index is available.
#' If an bgzip index is supplied, the file at \code{path} should be bgzip-compressed.
#' This index is mandatory if \code{index} is supplied and \code{path} is bgzip-compressed.
#' @param sequence.type String specifying the sequence type, should be one of \code{"DNA"}, \code{"RNA"} or \code{"AA"}.
#'
#' @details
#' The FastaWrapper class is a subclass of a \linkS4class{CompressedIndexedWrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}, \code{index}, \code{compression}.
#'
#' The \code{stageObject} method for FastaWrapper classes will check the FASTA file by reading the first few lines 
#' and attempting to import it into an \linkS4class{XStringSet} object using the relevant \pkg{Biostrings} functions, e.g., \code{\link{readDNAStringSet}}.
#' If an index is supplied, the method will check its validity via \code{\link{scanFaIndex}}.
#' 
#' @author Aaron Lun
#'
#' @return A FastaWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a FASTA file.
#' tmp <- tempfile(fileext=".fa")
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
#' show,FastaWrapper-method
#' loadFastaWrapper
#' @export
FastaWrapper <- function(path, sequence.type="DNA", compression=NULL, index=NULL, gzindex=NULL) {
    construct_fa_wrapper(
        path, 
        compression=compression, 
        index=index, 
        gzindex=gzindex,
        wrapper_class="FastaWrapper",
        sequence.type=sequence.type
    )
}

#' @importFrom Biostrings readAAStringSet readRNAStringSet readDNAStringSet
fa_validator <- function(x) {
    switch(x@sequence.type, 
        DNA=readDNAStringSet,
        RNA=readRNAStringSet,
        AA=readAAStringSet
    )
}

#' @importFrom Rsamtools FaFile scanFaIndex
fa_index_validator <- function(x) {
    if (!is.null(x@index)) {
        gzindex <- x@index@path
        if (x@compression == "bgzip") {
            if (is.null(x@gzindex)) {
                stop("indexing requires both 'index' and 'gzindex' when compression is 'bgzip'")
            }
            gzindex <- x@gzindex@path
        } else if (x@compression != 'none') {
            stop("indexing only works with uncompressed or bgzip-compressed files")
        }
        handle <- FaFile(x@path, index=x@index@path, gzindex=gzindex)
        scanFaIndex(handle)
    }
}

#' @export
#' @importFrom alabaster.base .stageObject stageObject .writeMetadata .processMetadata
setMethod("stageObject", "FastaWrapper", function(x, dir, path, child=FALSE) {
    fa_validator(x)(x@path, format="fasta", nrec=10) # reading the first few records to validate them.
    fa_index_validator(x)

    info <- save_fa_wrapper(x, dir, path, fname="file.fa", sequence.type=x@sequence.type)
    list(
        "$schema" = "fasta_file/v1.json",
        path = info$path,
        fasta_file = info$inner
    )
})

#' @export
setMethod("show", "FastaWrapper", function(object) {
    cat(class(object)[1], "object containing", object@sequence.type, "sequences\n")
    show_fa_wrapper(object)
})

#' @export
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
loadFastaWrapper <- function(meta, project) {
    load_fa_wrapper(
        meta$path, 
        meta$fasta_file, 
        project, 
        constructor=FastaWrapper,
        sequence.type=meta$fasta_file$type
    )
}

construct_fa_wrapper <- function(path, compression, index, gzindex, wrapper_class, sequence.type, ...) {
    output <- construct_compressed_indexed_wrapper(
        path, 
        compression=compression, 
        index=index, 
        wrapper_class=wrapper_class,
        index_constructor=FaIndexWrapper,
        sequence.type=sequence.type,
        ...
    )

    if (!is.null(gzindex)) {
        if (is.character(gzindex)) {
            gzindex <- BgzipIndexWrapper(gzindex)
        }
        output@gzindex <- gzindex
    }
    
    output
}

#' @importFrom S4Vectors coolcat metadata
show_fa_wrapper <- function(object) {
    cat("path:", object@path, "\n")
    cat("compression:", object@compression, "\n")
    show_with_index(object)
    if (object@compression == "bgzip") {
        if (length(object@gzindex)) {
            cat("gzindex:", object@gzindex@path, "\n")
        } else {
            cat("gzindex: <none>\n")
        }
    }
    coolcat("metadata(%i): %s", names(metadata(object)))
}

#' @importFrom alabaster.base .stageObject .writeMetadata
save_fa_wrapper <- function(x, dir, path, fname, sequence.type, ...) {
    meta <- save_compressed_indexed_wrapper(x, dir, path, fname=fname, index_class="FaIndexWrapper", type=sequence.type, ...)

    gzindex <- x@gzindex
    if (!is.null(gzindex)) {
        if (!is(gzindex, "BgzipIndexWrapper")) {
            stop("expected the index to be a '", "BgzipIndexWrapper", "' instance")
        }
        gzimeta <- .stageObject(gzindex, dir, paste0(path, "/gzindex"), parent=basename(meta$path), child=TRUE)
        meta$inner$gzindex <- list(resource=.writeMetadata(gzimeta, dir))
    }

    meta
}

#' @importFrom alabaster.base acquireMetadata .loadObject
load_fa_wrapper <- function(path, inner_meta, project, constructor, sequence.type, ...) {
    output <- load_compressed_indexed_wrapper(path, inner_meta, project, constructor, sequence.type=sequence.type, ...)

    gzindex <- NULL
    if ("gzindex" %in% names(inner_meta)) {
        gzimeta <- acquireMetadata(project, inner_meta$gzindex$resource$path)
        gzindex <- .loadObject(gzimeta, project)
    }
    output@gzindex <- gzindex

    output
}
