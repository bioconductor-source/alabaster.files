#' @export
FastqWrapper <- function(path, encoding, sequence.type="DNA", compression=NULL, index=NULL, gzindex=NULL) {
    construct_fa_wrapper(path, 
        compression=compression, 
        index=index, 
        gzindex=gzindex,
        wrapper_class="FastqWrapper", 
        sequence.type=sequence.type,
        encoding=encoding
    )
}

#' @export
setMethod("stageObject", "FastqWrapper", function(x, dir, path, child=FALSE) {
    fa_validator(x)(x@path, format="fastq", nrec=10, with.qualities=TRUE) # reading the first few records to validate them.

    info <- save_fa_wrapper(x, dir, path, fname="file.fastq", sequence.type=x@sequence.type, quality_encoding=x@encoding)
    list(
        "$schema" = "fastq_file/v1.json",
        path = info$path,
        fastq_file = info$inner
    )
})

setMethod("showheader", "FastqWrapper", function(object) {
    cat(class(object)[1], "object containing", object@sequence.type, "sequences with ", object@encoding, "quality scores\n")
    show_fa_wrapper(object)
})

#' @export
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
loadFastqWrapper <- function(meta, project) {
    load_fa_wrapper(
        meta$path, 
        meta$fastq_file, 
        project, 
        constructor=FastqWrapper, 
        sequence.type=meta$fastq_file$type,
        encoding=meta$fastq_file$quality_encoding
    )
}
