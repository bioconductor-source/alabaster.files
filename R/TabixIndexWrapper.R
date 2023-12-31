#' Wrapper for a Tabix file
#'
#' This class is deprecated and only listed here for back-compatibility purposes.
#'
#' @param path String containing the path to a Tabix file.
#'
#' @details
#' The TabixIndexWrapper class is a subclass of a \linkS4class{Wrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}.
#' 
#' @author Aaron Lun
#'
#' @return A TabixIndexWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a Tabix file.
#' test_tbx <- system.file("extdata", "example.gtf.gz.tbi", package="Rsamtools")
#'
#' # Creating a TabixIndexWrapper.
#' wrapped <- TabixIndexWrapper(test_tbx)
#' wrapped
#'
#' # Staging the TabixIndexWrapper.
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
#' TabixIndexWrapper-class
#' stageObject,TabixIndexWrapper-method
#' loadTabixIndexWrapper
#' @export
TabixIndexWrapper <- function(path) {
    new("TabixIndexWrapper", path=path)
}

#' @export
setMethod("stageObject", "TabixIndexWrapper", function(x, dir, path, child=FALSE, parent="file") {
    info <- save_wrapper(x, dir, path, fname=paste0(parent, ".tbi"))
    list(
        "$schema" = "tabix_index_file/v1.json",
        path = info$path,
        tabix_index_file = info$inner
    )
})

#' @export
loadTabixIndexWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$tabix_index_file, project, constructor=TabixIndexWrapper)
}
