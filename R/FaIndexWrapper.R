#' @export
FaIndexWrapper <- function(path) {
    new("FaIndexWrapper", path=path)
}

#' @export
setMethod("stageObject", "FaIndexWrapper", function(x, dir, path, child=FALSE, parent="file.fa", parent.compression="none") {
    info <- save_wrapper(x, dir, path, fname=paste0(parent, ".fai"))
    list(
        "$schema" = "fa_index_file/v1.json",
        path = info$path,
        fa_index_file = info$inner
    )
})

#' @export
loadFaIndexWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$fa_index_file, project, constructor=FaIndexWrapper)
}
