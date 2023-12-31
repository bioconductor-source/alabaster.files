#' Reference to a bigWig file
#'
#' Reference to a bigWig file, for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a bigWig file.
#'
#' @author Aaron Lun
#'
#' @return A BigWigFileReference instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a bigWig file.
#' test_path <- system.file("tests", "test.bw", package = "rtracklayer")
#'
#' # Creating a BigWigFileReference.
#' wrapped <- BigWigFileReference(test_path)
#' wrapped
#'
#' # Staging the BigWigFileReference.
#' dir <- tempfile()
#' saveObject(wrapped, dir)
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' readObject(dir)
#' 
#' @docType class
#' @aliases
#' BigWigFileReference-class
#' saveObject,BigWigFileReference-method
#' readBigWigFileReference
#' BigWigWrapper
#' BigWigWrapper-class
#' stageObject,BigWigWrapper-method
#' loadBigWigWrapper
#' @export
BigWigFileReference <- function(path) {
    new("BigWigFileReference", path=path)
}

#' @export
setMethod("saveObject", "BigWigFileReference", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)
    transfer_file(x@path, file.path(path, "file.bw")) 
    saveObjectFile(path, "bigwig_file", list(bigwig_file=list(version="1.0")))
    invisible(NULL)
})

#' @export
readBigWigFileReference <- function(path, ...) {
    BigWigFileReference(file.path(path, "file.bw"))
}
