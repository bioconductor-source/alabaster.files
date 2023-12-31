#' @export
BamIndexWrapper <- function(path) {
    new("BamIndexWrapper", path=path)
}

#' @export
setMethod("stageObject", "BamIndexWrapper", function(x, dir, path, child=FALSE, parent="file.bam") {
    info <- save_wrapper(x, dir, path, fname=paste0(parent, ".bai"))
    list(
        "$schema" = "bam_index_file/v1.json",
        path = info$path,
        bam_index_file = info$inner
    )
})

#' @export
loadBamIndexWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$bam_index_file, project, constructor=BamIndexWrapper)
}
