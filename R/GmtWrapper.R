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
