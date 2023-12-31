#' @export
BamWrapper <- function(path, index=NULL) {
    construct_indexed_wrapper(path, index=index, wrapper_class="BamWrapper", index_constructor=BamIndexWrapper)
}

#' @export
setMethod("stageObject", "BamWrapper", function(x, dir, path, child=FALSE) {
    Rsamtools::scanBamHeader(x@path) # validating it by scanning the header.
    if (!is.null(x@index)) {
        handle <- Rsamtools::BamFile(x@path, index=x@index@path)
        Rsamtools::idxstatsBam(handle)
    }

    info <- save_indexed_wrapper(x, dir, path, fname="file.bam", index_class="BamIndexWrapper")
    list(
        "$schema" = "bam_file/v1.json",
        path = info$path,
        bam_file = info$inner
    )
})

#' @export
loadBamWrapper <- function(meta, project) {
    load_indexed_wrapper(meta$path, meta$bam_file, project, constructor=BamWrapper)
}
