#' @export
GffWrapper <- function(path, compression=NULL, index=NULL, format=NULL) {
    if (is.null(format)) {
        if (grepl(gff2.pattern, path)) {
            format <- "GFF2"
        } else if (grepl(gff3.pattern, path)) {
            format <- "GFF3"
        } else {
            stop("cannot automatically determine GFF format from file extension")
        }
    }
    construct_compressed_indexed_wrapper(path, compression=compression, index=index, wrapper_class="GffWrapper", index_constructor=TabixIndexWrapper, format=format)
}

ext.pattern <- "\\.%s(\\.bz2|\\.gz|\\.bgz)?$"
gff2.pattern <- sprintf(ext.pattern, "(gff2|gtf)")
gff3.pattern <- sprintf(ext.pattern, "gff3")

#' @export
setMethod("stageObject", "GffWrapper", function(x, dir, path, child=FALSE) {
    ext <- if (x@format == "GFF2") "gff2" else "gff3"

    # Checking that the importer runs without error.
    top.lines <- read_first_few_lines(x@path, compression=x@compression, comment="#")
    tmp <- tempfile()
    write(top.lines, file=tmp) # textConnection doesn't work as import.gff requires a seekable connection.
    on.exit(unlink(tmp))
    validator <- if (x@format=="GFF3") rtracklayer::import.gff3 else rtracklayer::import.gff2
    validator(tmp)

    if (!is.null(x@index)) {
        handle <- Rsamtools::TabixFile(x@path, index=x@index@path)
        Rsamtools::headerTabix(handle)
    }

    info <- save_compressed_indexed_wrapper(x, dir, path, fname=paste0("file.", ext), index_class="TabixIndexWrapper")

    meta <- list(
        "$schema" = "gff_file/v1.json",
        path = info$path,
        gff_file = info$inner
    )
    meta$gff_file$format <- x@format

    meta
})

setMethod("showheader", "GffWrapper", function(object) {
    cat(class(object)[1], "object in the", object@format, "format\n")
})

#' @export
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
loadGffWrapper <- function(meta, project) {
    load_compressed_indexed_wrapper(meta$path, meta$gff_file, project, constructor=GffWrapper, format=meta$gff_file$format)
}
