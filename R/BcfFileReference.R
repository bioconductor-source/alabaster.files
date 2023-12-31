#' Reference to a BCF file
#'
#' Reference to a BCF file, for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a BCF file.
#' @param index String specifying the path to an index file in tabix or CSI format, or \code{NULL} if no index is available.
#'
#' @author Aaron Lun
#'
#' @return A BcfFileReference instance that can be used in \code{\link{saveObject}}.
#'
#' @examples
#' # Using Rsamtools's example file.
#' fl <- system.file("extdata", "ex1.bcf.gz", package="Rsamtools")
#'
#' # Creating a BcfFileReference.
#' wrapped <- BcfFileReference(fl)
#' wrapped
#'
#' # Fetching the path information:
#' path(wrapped)
#' wrapped$index
#'
#' # Staging the BcfFileReference.
#' dir <- tempfile()
#' saveObject(wrapped, dir)
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' readObject(dir)
#' 
#' @docType class
#' @aliases
#' BcfFileReference-class
#' saveObject,BcfFileReference-method
#' readBcfFileReference
#' VcfWrapper
#' VcfWrapper-class
#' stageObject,VcfWrapper-method
#' loadVcfWrapper
#' @export
BcfFileReference <- function(path, index=NULL) {
    new("BcfFileReference", path=path, index=index)
}

#' @export
setMethod("saveObject", "BcfFileReference", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)
    dest <- file.path(path, "file.bcf")
    transfer_file(x@path, dest)

    if (!is.null(x@index)) {
        transfer_index_file(x@index, dest, extensions=c("tbi", "csi"), format="BCF")
    }
    saveObjectFile(path, "bcf_file", list(bcf_file=list(version="1.0")))
    invisible(NULL)
})

#' @export
readBcfFileReference <- function(path, metadata, ...) {
    bpath <- file.path(path, "file.bcf")
    index <- choose_available_index(bpath, extensions=c("tbi", "csi"))
    BcfFileReference(bpath, index=index)
}
