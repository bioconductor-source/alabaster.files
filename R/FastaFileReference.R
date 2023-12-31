#' Reference to a FASTA file
#'
#' Reference to a FASTA file, for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a Gzip- or BGZF-compressed FASTA file.
#' @param faindex String specifying the path to an FASTA index file, or \code{NULL} if no index is available.
#' If an index is supplied, the file at \code{path} should be BGZF-compressed, and \code{gzindex} should also be supplied.
#' @param gzindex String specifying the path to a BGZF index file, or \code{NULL} if no index is available.
#' If an index is supplied, the file at \code{path} should be BGZF-compressed, and \code{faindex} should also be supplied.
#' @param seqtype String specifying the sequence type.
#' This should be one of \code{"DNA"}, \code{"RNA"}, \code{"AA"} or \code{"custom"}.
#'
#' @author Aaron Lun
#'
#' @return A FastaFileReference instance that can be used in \code{\link{saveObject}}.
#'
#' @examples
#' # Mocking up a FASTA file.
#' tmp <- tempfile(fileext=".fa.gz")
#' write(">FOOBAR\nacgtacgt", gzfile(tmp))
#'
#' # Creating a FastaFileReference.
#' wrapped <- FastaFileReference(tmp)
#' wrapped
#'
#' # Saving to disk:
#' dir <- tempfile()
#' saveObject(wrapped, dir)
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' readObject(dir)
#' 
#' @docType class
#' @aliases
#' FastaFileReference-class
#' saveObject,FastaFileReference-method
#' readFastaFileReference
#' FastaWrapper
#' FastaWrapper-class
#' stageObject,FastaWrapper-method
#' show,FastaWrapper-method
#' loadFastaWrapper
#' FaIndexWrapper
#' FaIndexWrapper-class
#' stageObject,FaIndexWrapper-method
#' loadFaIndexWrapper
#' @export
FastaFileReference <- function(path, seqtype="DNA", faindex=NULL, gzindex=NULL) {
    new("FastaFileReference", path=path, seqtype=seqtype, faindex=faindex, gzindex=gzindex)
}

#' @export
setMethod("saveObject", "FastaFileReference", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)

    indexed <- !is.null(x@faindex)
    if (!indexed) {
        transfer_file(x@path, file.path(path, "file.fasta.gz"))
    } else {
        dest <- file.path(path, "file.fasta.bgz")
        transfer_file(x@path, dest)
        transfer_file(x@faindex, file.path(path, "file.fasta.fai"))
        transfer_file(x@gzindex, paste0(dest, ".gzi"))
    }

    saveObjectFile(path, "fasta_file", list(fasta_file=list(version="1.0", sequence_type=x@seqtype, indexed=indexed)))
    invisible(NULL)
})

#' @export
readFastaFileReference <- function(path, metadata, ...) {
    if (!isTRUE(metadata$fasta_file$indexed)) {
        FastaFileReference(
            file.path(path, "file.fasta.gz"), 
            seqtype=metadata$fasta_file$sequence_type
        )
    } else {
        dest <- file.path(path, "file.fasta.bgz")
        FastaFileReference(
            dest,
            seqtype=metadata$fasta_file$sequence_type,
            faindex=file.path(path, "file.fasta.fai"),
            gzindex=paste0(dest, ".gzi")
        )
    }
}
