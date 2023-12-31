#' @export
VcfWrapper <- function(path, compression=NULL, index=NULL, header_only=FALSE) {
    construct_compressed_indexed_wrapper(path, compression=compression, index=index, wrapper_class="VcfWrapper", index_constructor=TabixIndexWrapper, header_only=header_only)
}

#' @export
setMethod("stageObject", "VcfWrapper", function(x, dir, path, child=FALSE) {
    # Checking that the importer runs without error.
    top.lines <- read_first_few_lines(x@path, compression=x@compression, comment="#")

    tmp <- tempfile()
    write(top.lines, file=tmp) # textConnection doesn't work here.
    on.exit(unlink(tmp))
    VariantAnnotation::readVcf(tmp)

    if (!is.null(x@index)) {
        handle <- Rsamtools::TabixFile(x@path, index=x@index@path)
        Rsamtools::headerTabix(handle)
    }

    info <- save_compressed_indexed_wrapper(x, dir, path, fname="file.vcf", index_class="TabixIndexWrapper", header_only=x@header_only)
    list(
        "$schema" = "vcf_file/v1.json",
        path = info$path,
        vcf_file = info$inner
    )
})

setMethod("showheader", "VcfWrapper", function(object) {
    cat(class(object)[1], "object")
    if (object@header_only) {
        cat(" (header only)\n")
    } else {
        cat("\n")
    }
})

#' @export
loadVcfWrapper <- function(meta, project) {
    load_compressed_indexed_wrapper(meta$path, meta$vcf_file, project, constructor=VcfWrapper)
}
