#' Reference to a GFF file
#'
#' Reference to a GFF2/3 file, for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a Gzip- or BGZF-compressed GFF file.
#' The format is automatically detected from the file extension (\code{.gff2}, \code{.gff3} or \code{.gtf}).
#' @param index String specifying the path to a tabix file in tabix format, or \code{NULL} if no index is available.
#' If supplied, \code{path} should be coordinate-sorted and BGZF-compressed.
#'
#' @author Aaron Lun
#'
#' @return A GffFileReference instance that can be used in \code{\link{saveObject}}.
#'
#' @examples
#' # Using rtracklayer's example GFF file.
#' src <- system.file("tests", "genes.gff3", package = "rtracklayer")
#' fl <- tempfile(fileext=".gff3.gz")
#' writeLines(con=gzfile(fl), readLines(src))
#'
#' # Creating a GffFileReference.
#' wrapped <- GffFileReference(fl)
#' wrapped
#'
#' # Saving it:
#' dir <- tempfile()
#' saveObject(wrapped, dir)
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' readObject(dir)
#' 
#' @docType class
#' @aliases
#' GffFileReference-class
#' saveObject,GffFileReference-method
#' readGffFileReference
#' GffWrapper
#' GffWrapper-class
#' stageObject,GffWrapper-method
#' loadGffWrapper
#' @export
GffFileReference <- function(path, index=NULL) {
    new("GffFileReference", path=path, index=index)
}

#' @export
setMethod("saveObject", "GffFileReference", function(x, path, ...) {
    dir.create(path, showWarnings=FALSE)
    indexed <- !is.null(x@index)

    if (endsWith(x@path, ".gff3.bgz") || endsWith(x@path, ".gff3.gz")) {
        format <- "GFF3"
    } else if (endsWith(x@path, ".gff2.bgz") || endsWith(x@path, ".gff2.gz") || endsWith(x@path, ".gtf.bgz") || endsWith(x@path, ".gtf.gz")) {
        format <- "GFF2"
    } else {
        stop("could not determine GFF format from the file extension")
    }

    if (!indexed) {
        transfer_file(x@path, file.path(path, paste0("file.", tolower(format), ".gz")))
    } else {
        dest <- file.path(path, paste0("file.", tolower(format), ".bgz"))
        transfer_file(x@path, dest)
        transfer_index_file(x@index, dest, extensions=c("tbi", "csi"), format="BED")
    }

    saveObjectFile(path, "gff_file", list(gff_file=list(version="1.0", indexed=indexed, format=format)))
    invisible(NULL)
})

#' @export
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
readGffFileReference <- function(path, metadata, ...) {
    format <- metadata$gff_file$format
    if (!isTRUE(metadata$gff_file$indexed)) {
        GffFileReference(file.path(path, paste0("file.", tolower(format), ".gz")))
    } else {
        bpath <- file.path(path, paste0("file.", tolower(format), ".bgz"))
        index <- choose_available_index(bpath, extensions=c("tbi", "csi"))
        GffFileReference(path=bpath, index=index)
    }
}
