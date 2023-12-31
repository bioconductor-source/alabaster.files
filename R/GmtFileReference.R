#' Reference to a GMT file
#'
#' Reference to a GMT file, for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a Gzip-compressed GMT file.
#'
#' @author Aaron Lun
#'
#' @return A GmtFileReference instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a GMT file.
#' tmp <- tempfile(fileext=".gmt.gz")
#' write("SET1\tdescription\tgene1\tgene2\tgene3", file=gzfile(tmp))
#'
#' # Creating a GmtFileReference.
#' wrapped <- GmtFileReference(tmp)
#' wrapped
#'
#' # Saving to disk:
#' dir <- tempfile()
#' saveObject(wrapped, dir)
#'
#' # Loading it back again:
#' readObject(dir)
#' 
#' @docType class
#' @aliases
#' GmtFileReference-class
#' saveObject,GmtFileReference-method
#' readGmtFileReference
#' GmtWrapper
#' GmtWrapper-class
#' stageObject,GmtWrapper-method
#' loadGmtWrapper
#' @export
GmtFileReference <- function(path) {
    new("GmtFileReference", path=path)
}

#' @export
setMethod("saveObject", "GmtFileReference", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)
    transfer_file(x@path, file.path(path, paste0("file.gmt.gz")))
    saveObjectFile(path, "gmt_file", list(gmt_file=list(version="1.0")))
    invisible(NULL)
})

#' @export
readGmtFileReference <- function(path, metadata, ...) {
    GmtFileReference(file.path(path, "file.gmt.gz"))
}
