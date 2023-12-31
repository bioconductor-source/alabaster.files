#' @export
BedWrapper <- function(path, compression=NULL, index=NULL) {
    construct_compressed_indexed_wrapper(path, compression=compression, index=index, wrapper_class="BedWrapper", index_constructor=TabixIndexWrapper)
}

#' @export
setMethod("stageObject", "BedWrapper", function(x, dir, path, child=FALSE, validate=TRUE) {
    top.lines <- read_first_few_lines(x@path, compression=x@compression)

    format <- 'BED'
    header <- top.lines[1]
    if (length(header)) {
        if (length(strsplit(header, "\t")[[1]]) == 15) {
            format <- 'BED15'
        }
    }

    # Checking that the importer runs without error.
    con <- textConnection(top.lines)
    validator <- if (format=="BED") rtracklayer::import.bed else rtracklayer::import.bed15
    validator(con)

    if (!is.null(x@index)) {
        handle <- Rsamtools::TabixFile(x@path, index=x@index@path)
        Rsamtools::headerTabix(handle)
    }

    info <- save_compressed_indexed_wrapper(x, dir, path, fname="file.bed", index_class="TabixIndexWrapper")
    meta <- list(
        "$schema" = "bed_file/v1.json",
        path = info$path,
        bed_file = info$inner
    )

    meta$bed_file$format <- format

    meta
})

#' @export
loadBedWrapper <- function(meta, project) {
    load_compressed_indexed_wrapper(meta$path, meta$bed_file, project, constructor=BedWrapper)
}
