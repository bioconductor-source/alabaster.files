#' Wrapper for a bigBed file
#'
#' Wrap a bigBed file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a bigBed file.
#'
#' @details
#' The BigBedWrapper class is a subclass of a \linkS4class{Wrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}.
#' 
#' The \code{stageObject} method for BigBedWrapper classes will check the bigBed file by searching for the bigBed magic number,
#' i.e., 0x8789F2EB or its byte-reversed form.
#'
#' @author Aaron Lun
#'
#' @return A BigBedWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a bigBed file.
#' test_path <- system.file("tests", package = "rtracklayer")
#' test_bb <- file.path(test_path, "test.bb")
#'
#' # Creating a BigBedWrapper.
#' wrapped <- BigBedWrapper(test_bb)
#' wrapped
#'
#' # Staging the BigBedWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "my_bb")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "my_bb/file.bb")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' BigBedWrapper-class
#' stageObject,BigBedWrapper-method
#' loadBigBedWrapper
#' @export
BigBedWrapper <- function(path) {
    new("BigBedWrapper", path=path)
}

#' @export
setMethod("stageObject", "BigBedWrapper", function(x, dir, path, child=FALSE) {
    magic <- readBin(x@path, what=raw(), n=4)
    if (parse_magic_number(magic) != 0x8789F2EB && parse_magic_number(rev(magic)) != 0x8789F2EB) {
        stop("cannot detect BigBed magic number at the start of the file")
    }

    info <- save_wrapper(x, dir, path, fname="file.bb")
    list(
        "$schema" = "bigbed_file/v1.json",
        path = info$path,
        bigbed_file = info$inner
    )
})

#' @export
loadBigBedWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$bigbed_file, project, constructor=BigBedWrapper)
}
