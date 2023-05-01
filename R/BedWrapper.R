#' Wrapper for a BED file
#'
#' Wrap a BED file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a BED file.
#' @param compression String specifying the compression.
#' This should be one of \code{"none"}, \code{"gzip"}, \code{"bzip2"} or \code{"bgzip"}.
#' If \code{NULL}, this is inferred from the file's headers and suffix.
#' @param index String specifying the path to a tabix-formatted index file
#'
#' @author Aaron Lun
#'
#' @return A BedWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Mocking up a BED file.
#' tmp <- tempfile(fileext=".bed")
#' bed <- write("chr1\t2222\t33333", file=tmp)
#'
#' # Creating a BedWrapper.
#' wrapped <- BedWrapper(tmp)
#' wrapped
#'
#' # Staging the BedWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "my_bed")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "my_bed/file.bed")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' loadBedWrapper
#' BedWrapper-class
#' show,BedWrapper-method
#' stageObject,BedWrapper-method
#' @export
BedWrapper <- function(path, compression=NULL, index=NULL) {
    if (is.null(compression)) {
        compression <- guess_compression(path)
    }

    x <- new("BedWrapper", path=path, compression=compression)

    if (!is.null(index)) {
        if (is.character(index)) {
            index <- TabixWrapper(index)
        }
        x@index <- list(index)
    }

    x
}

#' @export
#' @importFrom S4Vectors coolcat metadata
setMethod("show", "BedWrapper", function(object) {
    cat("BedWrapper object\n")
    cat("path:", object@path, "\n")
    cat("compression:", object@compression, "\n")

    if (length(object@index)) {
        cat("index:", object@index[[1]]@path, "\n")
    } else {
        cat("index: (none)\n")
    }

    coolcat("metadata(%i): %s", names(metadata(object)))
})

#' @export
#' @importFrom alabaster.base .stageObject stageObject .writeMetadata .processMetadata
setMethod("stageObject", "BedWrapper", function(x, dir, path, child=FALSE) {
    dir.create(file.path(dir, path), showWarnings=FALSE, recursive=TRUE)

    target <- paste0(path, "/", "file.bed", compression_extension(x@compression))
    host <- file.path(dir, target)
    transfer_file(x@path, host)

    meta <- list(
        "$schema" = "bed_file/v1.json",
        path = target,
        bed_file = list(compression = x@compression)
    )

    header <- readLines(x@path, n = 1L)
    format <- 'BED'
    if (length(header)) {
        if (length(strsplit(header, "\t")[[1]]) == 15) {
            format <- 'BED15'
        }
    }
    meta$bed_file$format <- format

    index <- x@index
    if (length(index)) {
        if (!is(index[[1]], "TabixWrapper")) {
            stop("BED file index should be a tabix file")
        }
        imeta <- .stageObject(index[[1]], dir, paste0(path, "index"), child=TRUE)
        meta$bed_file$index <- list(resource=.writeMetadata(imeta, dir))
    }

    meta$bed_file$other_data <- .processMetadata(x, dir, path, "other") 

    meta
})

#' @export
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
loadBedWrapper <- function(meta, project) {
    fpath <- acquireFile(project, meta$path)

    index <- NULL
    if ("index" %in% names(meta$bed_file)) {
        imeta <- acquireMetadata(project, meta$bed_file$index$resource$path)
        index <- .loadObject(imeta, project)
    }

    output <- BedWrapper(fpath, compression=meta$bed_file$compression, index=index)
    .restoreMetadata(output, mcol.data=NULL, meta.data=meta$bed_file$other_data, project)
}
