#' Reference to a bigBed file
#'
#' Reference to a bigBed file, for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a bigBed file.
#'
#' @author Aaron Lun
#'
#' @return A BigBedFileReference instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a bigBed file.
#' test_path <- system.file("tests", "test.bb", package = "rtracklayer")
#'
#' # Creating a BigBedFileReference.
#' wrapped <- BigBedFileReference(test_path)
#' wrapped
#'
#' # Staging the BigBedFileReference.
#' dir <- tempfile()
#' saveObject(wrapped, dir)
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' readObject(dir)
#' 
#' @docType class
#' @aliases
#' BigBedFileReference-class
#' saveObject,BigBedFileReference-method
#' readBigBedFileReference
#' BigBedWrapper
#' BigBedWrapper-class
#' stageObject,BigBedWrapper-method
#' loadBigBedWrapper
#' @export
BigBedFileReference <- function(path) {
    new("BigBedFileReference", path=path)
}

#' @export
setMethod("saveObject", "BigBedFileReference", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)
    transfer_file(x@path, file.path(path, "file.bb")) 
    saveObjectFile(path, "bigbed_file", list(bigbed_file=list(version="1.0")))
    invisible(NULL)
})

#' @export
readBigBedFileReference <- function(path, ...) {
    BigBedFileReference(file.path(path, "file.bb"))
}
