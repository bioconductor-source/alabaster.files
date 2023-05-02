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
