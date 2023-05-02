#' Wrapper for a GMT file
#'
#' Wrap a GMT file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a GMT file.
#' @param compression String specifying the compression.
#' This should be one of \code{"none"}, \code{"gzip"}, \code{"bzip2"} or \code{"bgzip"}.
#' If \code{NULL}, this is inferred from the file's headers and suffix.
#'
#' @details
#' The GmtWrapper class is a subclass of a \linkS4class{CompressedWrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}, \code{compression}.
#' 
#' The \code{stageObject} method for GmtWrapper classes will check the GMT file by reading the first few lines 
#' and checking that each line contains at least three tab-separated fields.
#'
#' @author Aaron Lun
#'
#' @return A GmtWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a GMT file.
#' tmp <- tempfile(fileext=".gmt")
#' write("SET1\tdescription\tgene1\tgene2\tgene3", file=tmp)
#'
#' # Creating a GmtWrapper.
#' wrapped <- GmtWrapper(tmp)
#' wrapped
#'
#' # Staging the GmtWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "my_gmt")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "my_gmt/file.gmt")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' GmtWrapper-class
#' stageObject,GmtWrapper-method
#' loadGmtWrapper
#' @export
GmtWrapper <- function(path, compression=NULL) {
    construct_compressed_wrapper(path, compression=compression, wrapper_class="GmtWrapper")
}

#' @export
setMethod("stageObject", "GmtWrapper", function(x, dir, path, child=FALSE) {
    top.lines <- read_first_few_lines(x@path, compression=x@compression)
    if (!all(lengths(strsplit(top.lines, "\t")) >= 3)) {
        stop("expected at least three tab-separated fields in each line of a GMT file")
    }

    info <- save_compressed_wrapper(x, dir, path, fname="file.gmt")
    list(
        "$schema" = "gmt_file/v1.json",
        path = info$path,
        gmt_file = info$inner
    )
})

#' @export
loadGmtWrapper <- function(meta, project) {
    load_compressed_wrapper(meta$path, meta$gmt_file, project, constructor=GmtWrapper)
}
