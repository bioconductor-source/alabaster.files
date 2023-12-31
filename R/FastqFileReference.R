#' Reference to a FASTQ file
#'
#' Reference to a FASTQ file, for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a Gzip- or BGZF-compressed FASTQ file.
#' @param qualtype String specifying the type of the quality strings.
#' This should be one of \code{"phred"} or \code{"solexa"}.
#' @param qualoffset Integer specifying the encoding offset for the quality strings.
#' This is only used when \code{qualtype="phred"}, in which case it should either be 33 or 64.
#' @inheritParams FastaFileReference
#' @author Aaron Lun
#'
#' @return A FastqFileReference instance that can be used in \code{\link{saveObject}}.
#'
#' @examples
#' # Mocking up a FASTQ file.
#' tmp <- tempfile(fileext=".fq.gz")
#' write("@FOOBAR\nacgtacgt\n+134987382", gzfile(tmp))
#'
#' # Creating a FastqFileReference.
#' wrapped <- FastqFileReference(tmp)
#' wrapped
#'
#' # Staging the FastqFileReference.
#' dir <- tempfile()
#' saveObject(wrapped, dir)
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' readObject(dir)
#' 
#' @docType class
#' @aliases
#' FastqFileReference-class
#' saveObject,FastqFileReference-method
#' readFastqFileReference
#' FastqWrapper
#' FastqWrapper-class
#' stageObject,FastqWrapper-method
#' show,FastqWrapper-method
#' loadFastqWrapper
#' @export
FastqFileReference <- function(path, seqtype="DNA", qualtype="phred", qualoffset=33, faindex=NULL, gzindex=NULL) {
    new("FastqFileReference", path=path, seqtype=seqtype, qualtype=qualtype, qualoffset=as.integer(qualoffset), faindex=faindex, gzindex=gzindex)
}

#' @export
setMethod("saveObject", "FastqFileReference", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)

    indexed <- !is.null(x@faindex)
    if (!indexed) {
        transfer_file(x@path, file.path(path, "file.fastq.gz"))
    } else {
        dest <- file.path(path, "file.fastq.bgz")
        transfer_file(x@path, dest)
        transfer_file(x@faindex, file.path(path, "file.fastq.fai"))
        transfer_file(x@gzindex, paste0(dest, ".gzi"))
    }

    meta <- list(version="1.0", sequence_type=x@seqtype, quality_type=x@qualtype, indexed=indexed)
    if (x@qualtype == "phred") {
        meta$quality_offset <- x@qualoffset
    }

    saveObjectFile(path, "fastq_file", list(fastq_file=meta))
    invisible(NULL)
})

#' @export
readFastqFileReference <- function(path, metadata, ...) {
    offset <- metadata$fastq_file$quality_offset
    if (is.null(offset)) {
        offset <- NA_character_
    }

    if (!isTRUE(metadata$fastq_file$indexed)) {
        FastqFileReference(
            file.path(path, "file.fastq.gz"), 
            seqtype=metadata$fastq_file$sequence_type,
            qualtype=metadata$fastq_file$quality_type,
            qualoffset=offset
        )
    } else {
        dest <- file.path(path, "file.fastq.bgz")
        FastqFileReference(
            dest,
            faindex=file.path(path, "file.fastq.fai"),
            gzindex=paste0(dest, ".gzi"),
            seqtype=metadata$fastq_file$sequence_type,
            qualtype=metadata$fastq_file$quality_type,
            qualoffset=offset
        )
    }
}
