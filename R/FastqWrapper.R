#' Wrapper for a FASTQ file
#'
#' Wrap a FASTQ file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a FASTQ file.
#' @param encoding String specifying the encoding of the quality strings.
#' This should be one of \code{"phred"}, \code{"solexa"} or \code{"illumina"}.
#' @param compression String specifying the compression.
#' This should be one of \code{"none"}, \code{"gzip"}, \code{"bzip2"} or \code{"bgzip"}.
#' If \code{NULL}, this is inferred from the file's headers and suffix.
#' @param index String specifying the path to an faidx file, or \code{NULL} if no index is available.
#' If an index is supplied, the file should be uncompressed or bgzip-compressed.
#' @param sequence.type String specifying the sequence type, should be one of \code{"DNA"}, \code{"RNA"} or \code{"AA"}.
#'
#' @details
#' The FastqWrapper class is a subclass of a \linkS4class{CompressedIndexedWrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}, \code{index}, \code{compression}.
#' 
#' The \code{stageObject} method for FastqWrapper classes will check the FASTQ file by reading the first few lines 
#' and attempting to import it into an \linkS4class{XStringSet} object using the relevant \pkg{Biostrings} functions, e.g., \code{\link{readDNAStringSet}}.
#'
#' @author Aaron Lun
#'
#' @return A FastqWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a FASTQ file.
#' tmp <- tempfile(fileext=".fq")
#' write("@FOOBAR\nacgtacgt\n+134987382", tmp)
#'
#' # Creating a FastqWrapper.
#' wrapped <- FastqWrapper(tmp, encoding="phred")
#' wrapped
#'
#' # Staging the FastqWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "seq")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "seq/file.fastq")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' FastqWrapper-class
#' stageObject,FastqWrapper-method
#' loadFastqWrapper
#' @export
FastqWrapper <- function(path, encoding, sequence.type="DNA", compression=NULL, index=NULL) {
    construct_compressed_indexed_wrapper(path, 
        compression=compression, 
        index=index, 
        wrapper_class="FastqWrapper", 
        index_constructor=FaIndexWrapper,
        sequence.type=sequence.type,
        encoding=encoding
    )
}

#' @export
#' @importFrom alabaster.base .stageObject stageObject .writeMetadata .processMetadata
setMethod("stageObject", "FastqWrapper", function(x, dir, path, child=FALSE) {
    fa_validator(x)(x@path, format="fastq", nrec=10, with.qualities=TRUE) # reading the first few records to validate them.

    info <- save_compressed_indexed_wrapper(x, dir, path, fname="file.fastq", index_class="FaIndexWrapper", type=x@sequence.type, quality_encoding=x@encoding)
    list(
        "$schema" = "fastq_file/v1.json",
        path = info$path,
        fastq_file = info$inner
    )
})

setMethod("showheader", "FastqWrapper", function(object) {
    cat(class(object)[1], "object containing", object@sequence.type, "sequences with ", object@encoding, "quality scores\n")
})

#' @export
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
loadFastqWrapper <- function(meta, project) {
    load_compressed_indexed_wrapper(
        meta$path, 
        meta$fastq_file, 
        project, 
        constructor=FastqWrapper, 
        sequence.type=meta$fastq_file$type,
        encoding=meta$fastq_file$quality_encoding
    )
}
