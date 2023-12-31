#' Reference to a BED file
#'
#' Reference to a BED file, for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a Gzip- or BGZF-compressed BED file.
#' @param index String containing a path to a tabix file.
#' If supplied, \code{path} should be coordinate-sorted and BGZF-compressed.
#'
#' @author Aaron Lun
#'
#' @return A BedFileReference instance that can be used in \code{\link{saveObject}}.
#'
#' @examples
#' # Mocking up a BED file.
#' raw <- tempfile(fileext=".bed")
#' bed <- write("chr1\t2222\t33333", file=raw)
#' tmp <- tempfile(fileext=".bed.bgz")
#' Rsamtools::bgzip(raw, tmp)
#'
#' # Creating a BedFileReference.
#' wrapped <- BedFileReference(tmp)
#' wrapped
#'
#' # Extracting the paths:
#' path(wrapped)
#' wrapped$index
#'
#' # Saving it to disk.
#' dir <- tempfile()
#' saveObject(wrapped, dir)
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' readObject(dir)
#' 
#' @docType class
#' @aliases
#' BedFileReference-class
#' saveObject,BedFileReference-method
#' readBedFileReference 
#' BedWrapper
#' BedWrapper-class
#' stageObject,BedWrapper-method
#' loadBedWrapper
#' @export
BedFileReference <- function(path, index=NULL) {
    new("BedFileReference", path=path, index=index)
}

#' @export
setMethod("saveObject", "BedFileReference", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)
    indexed <- !is.null(x@index)

    if (!indexed) {
        transfer_file(x@path, file.path(path, "file.bed.gz")) 
    } else {
        dest <- file.path(path, "file.bed.bgz")
        transfer_file(x@path, dest)
        transfer_index_file(x@index, dest, extensions=c("tbi", "csi"), format="BED")
    }

    saveObjectFile(path, "bed_file", list(bed_file=list(version="1.0", indexed=indexed)))
    invisible(NULL)
})

#' @export
readBedFileReference <- function(path, metadata, ...) {
    if (!isTRUE(metadata$bed_file$indexed)) {
        return(BedFileReference(file.path(path, "file.bed.gz")))
    } else {
        bpath <- file.path(path, "file.bed.bgz")
        index <- choose_available_index(bpath, extensions=c("tbi", "csi"))
        BedFileReference(path=bpath, index=index)
    }
}
