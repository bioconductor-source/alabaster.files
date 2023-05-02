#' Wrapper for a bigWig file
#'
#' Wrap a bigWig file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a bigWig file.
#'
#' @details
#' The BigWigWrapper class is a subclass of a \linkS4class{Wrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}.
#'
#' The \code{stageObject} method for BigWigWrapper classes will check the bigWig file by searching for the bigWig magic number,
#' i.e., 0x888FFC26 or its byte-reversed form.
#' 
#' @author Aaron Lun
#'
#' @return A BigWigWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a bigWig file.
#' test_path <- system.file("tests", package = "rtracklayer")
#' test_bw <- file.path(test_path, "test.bw")
#'
#' # Creating a BigWigWrapper.
#' wrapped <- BigWigWrapper(test_bw)
#' wrapped
#'
#' # Staging the BigWigWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "my_bw")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "my_bw/file.bw")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' BigWigWrapper-class
#' stageObject,BigWigWrapper-method
#' loadBigWigWrapper
#' @export
BigWigWrapper <- function(path) {
    new("BigWigWrapper", path=path)
}

#' @export
setMethod("stageObject", "BigWigWrapper", function(x, dir, path, child=FALSE) {
    magic <- readBin(x@path, what=raw(), n=4)
    if (parse_magic_number(magic) != 0x888FFC26 && parse_magic_number(rev(magic)) != 0x888FFC26) {
        stop("cannot detect BigBed magic number at the start of the file")
    }

    info <- save_wrapper(x, dir, path, fname="file.bw")
    list(
        "$schema" = "bigwig_file/v1.json",
        path = info$path,
        bigwig_file = info$inner
    )
})

#' @export
loadBigWigWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$bigwig_file, project, constructor=BigWigWrapper)
}
