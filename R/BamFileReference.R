#' Reference to a BAM file
#'
#' Reference to a BAM file, for saving and reading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a BAM file.
#' @param index String specifying the path to a BAI or CSI index file, or \code{NULL} if no index is available.
#'
#' @author Aaron Lun
#'
#' @return A BamFileReference instance that can be used in \code{\link{saveObject}}.
#'
#' @examples
#' # Using a BAM file from Rsamtools.
#' fl <- system.file("extdata", "ex1.bam", package="Rsamtools", mustWork=TRUE)
#'
#' # Creating a BamFileReference.
#' wrapped <- BamFileReference(fl)
#' wrapped
#'
#' # Fetching the path information:
#' path(wrapped)
#' wrapped$index
#'
#' # Saving to disk:
#' dir <- tempfile()
#' saveObject(wrapped, dir)
#' list.files(dir, recursive=TRUE)
#'
#' # Reading it back again:
#' readObject(dir)
#' 
#' @docType class
#' @aliases
#' BamFileReference-class
#' saveObject,BamFileReference-method
#' readBamFileReference
#' BamWrapper
#' BamWrapper-class
#' stageObject,BamWrapper-method
#' loadBamWrapper
#' BamIndexWrapper
#' BamIndexWrapper-class
#' stageObject,BamIndexWrapper-method
#' loadBamIndexWrapper
#' @export
BamFileReference <- function(path, index=NULL) {
    new("BamFileReference", path=path, index=index)
}

#' @export
setMethod("saveObject", "BamFileReference", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)
    dest <- file.path(path, "file.bam")
    transfer_file(x@path, dest)

    if (!is.null(x@index)) {
        transfer_index_file(x@index, dest, extensions=c("bai", "csi"), format="BAM")
    }

    saveObjectFile(path, "bam_file", list(bam_file=list(version="1.0")))
    invisible(NULL)
})

#' @export
readBamFileReference <- function(path, metadata, ...) {
    bpath <- file.path(path, "file.bam")
    index <- choose_available_index(bpath, extensions=c("bai", "csi"))
    BamFileReference(bpath, index=index)
}
