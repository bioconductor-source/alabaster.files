#' Wrapper for a Tabix file
#'
#' Wrap a Tabix file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a Tabix file.
#'
#' @details
#' The TabixWrapper class is a subclass of a \linkS4class{Wrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}.
#' 
#' @author Aaron Lun
#'
#' @return A TabixWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a Tabix file.
#' test_tbx <- system.file("extdata", "example.gtf.gz.tbi", package="Rsamtools")
#'
#' # Creating a TabixWrapper.
#' wrapped <- TabixWrapper(test_tbx)
#' wrapped
#'
#' # Staging the TabixWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "tab")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "tab/file.tbi")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' TabixWrapper-class
#' stageObject,TabixWrapper-method
#' loadTabixWrapper
#' @export
TabixWrapper <- function(path) {
    new("TabixWrapper", path=path)
}

#' @export
setMethod("stageObject", "TabixWrapper", function(x, dir, path, child=FALSE, parent="file") {
    info <- save_wrapper(x, dir, path, fname=paste0(parent, ".tbi"))
    list(
        "$schema" = "tabix_file/v1.json",
        path = info$path,
        tabix_file = info$inner
    )
})

#' @export
loadTabixWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$tabix_file, project, constructor=TabixWrapper)
}
