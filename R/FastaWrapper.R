#' @export
FastaWrapper <- function(path, sequence.type="DNA", compression=NULL, index=NULL, gzindex=NULL) {
    construct_fa_wrapper(
        path, 
        compression=compression, 
        index=index, 
        gzindex=gzindex,
        wrapper_class="FastaWrapper",
        sequence.type=sequence.type
    )
}

fa_validator <- function(x) {
    switch(x@sequence.type, 
        DNA=Biostrings::readDNAStringSet,
        RNA=Biostrings::readRNAStringSet,
        AA= Biostrings::readAAStringSet
    )
}

fa_index_validator <- function(x) {
    if (!is.null(x@index)) {
        gzindex <- x@index@path
        if (x@compression == "bgzip") {
            if (is.null(x@gzindex)) {
                stop("indexing requires both 'index' and 'gzindex' when compression is 'bgzip'")
            }
            gzindex <- x@gzindex@path
        } else if (x@compression != 'none') {
            stop("indexing only works with uncompressed or bgzip-compressed files")
        }
        handle <- Rsamtools::FaFile(x@path, index=x@index@path, gzindex=gzindex)
        Rsamtools::scanFaIndex(handle)
    }
}

#' @export
#' @importFrom alabaster.base .stageObject stageObject .writeMetadata .processMetadata
setMethod("stageObject", "FastaWrapper", function(x, dir, path, child=FALSE) {
    fa_validator(x)(x@path, format="fasta", nrec=10) # reading the first few records to validate them.
    fa_index_validator(x)

    info <- save_fa_wrapper(x, dir, path, fname="file.fa", sequence.type=x@sequence.type)
    list(
        "$schema" = "fasta_file/v1.json",
        path = info$path,
        fasta_file = info$inner
    )
})

#' @export
setMethod("show", "FastaWrapper", function(object) {
    cat(class(object)[1], "object containing", object@sequence.type, "sequences\n")
    show_fa_wrapper(object)
})

#' @export
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
loadFastaWrapper <- function(meta, project) {
    load_fa_wrapper(
        meta$path, 
        meta$fasta_file, 
        project, 
        constructor=FastaWrapper,
        sequence.type=meta$fasta_file$type
    )
}

construct_fa_wrapper <- function(path, compression, index, gzindex, wrapper_class, sequence.type, ...) {
    output <- construct_compressed_indexed_wrapper(
        path, 
        compression=compression, 
        index=index, 
        wrapper_class=wrapper_class,
        index_constructor=FaIndexWrapper,
        sequence.type=sequence.type,
        ...
    )

    if (!is.null(gzindex)) {
        if (is.character(gzindex)) {
            gzindex <- BgzipIndexWrapper(gzindex)
        }
        output@gzindex <- gzindex
    }
    
    output
}

#' @importFrom S4Vectors coolcat metadata
show_fa_wrapper <- function(object) {
    cat("path:", object@path, "\n")
    cat("compression:", object@compression, "\n")
    show_with_index(object)
    if (object@compression == "bgzip") {
        if (length(object@gzindex)) {
            cat("gzindex:", object@gzindex@path, "\n")
        } else {
            cat("gzindex: <none>\n")
        }
    }
    coolcat("metadata(%i): %s", names(metadata(object)))
}

#' @importFrom alabaster.base .stageObject .writeMetadata
save_fa_wrapper <- function(x, dir, path, fname, sequence.type, ...) {
    meta <- save_compressed_indexed_wrapper(x, dir, path, fname=fname, index_class="FaIndexWrapper", type=sequence.type, ...)

    gzindex <- x@gzindex
    if (!is.null(gzindex)) {
        if (!is(gzindex, "BgzipIndexWrapper")) {
            stop("expected the index to be a '", "BgzipIndexWrapper", "' instance")
        }
        gzimeta <- .stageObject(gzindex, dir, paste0(path, "/gzindex"), parent=basename(meta$path), child=TRUE)
        meta$inner$gzindex <- list(resource=.writeMetadata(gzimeta, dir))
    }

    meta
}

#' @importFrom alabaster.base acquireMetadata .loadObject
load_fa_wrapper <- function(path, inner_meta, project, constructor, sequence.type, ...) {
    output <- load_compressed_indexed_wrapper(path, inner_meta, project, constructor, sequence.type=sequence.type, ...)

    gzindex <- NULL
    if ("gzindex" %in% names(inner_meta)) {
        gzimeta <- acquireMetadata(project, inner_meta$gzindex$resource$path)
        gzindex <- .loadObject(gzimeta, project)
    }
    output@gzindex <- gzindex

    output
}
