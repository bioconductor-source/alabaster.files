#' Wrapper for a Bgzip index file
#'
#' Wrap a Bgzip index file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a Bgzip index file.
#'
#' @details
#' The BgzipIndexWrapper class is a subclass of a \linkS4class{Wrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}.
#' 
#' @author Aaron Lun
#'
#' @return A BgzipIndexWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a FASTA index file.
#' input <- system.file("extdata", "ce2dict1.fa", package="Rsamtools")
#' temp <- tempfile(fileext=".fa.bgz")
#' copy <- Rsamtools::bgzip(input, dest=temp)
#' Rsamtools::indexFa(copy)
#'
#' # Creating a BgzipIndexWrapper.
#' wrapped <- BgzipIndexWrapper(paste0(copy, ".gzi"))
#' wrapped
#'
#' # Staging the BgzipIndexWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "tab")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "tab/file.fa.bgz.gzi")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' BgzipIndexWrapper-class
#' stageObject,BgzipIndexWrapper-method
#' loadBgzipIndexWrapper
#' @export
BgzipIndexWrapper <- function(path) {
    new("BgzipIndexWrapper", path=path)
}

#' @export
setMethod("stageObject", "BgzipIndexWrapper", function(x, dir, path, child=FALSE, parent="file.fa.bgz", parent.compression="none") {
    info <- save_wrapper(x, dir, path, fname=paste0(parent, ".gzi"))
    list(
        "$schema" = "bgzip_index_file/v1.json",
        path = info$path,
        bgzip_index_file = info$inner
    )
})

#' @export
loadBgzipIndexWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$bgzip_index_file, project, constructor=BgzipIndexWrapper)
}
