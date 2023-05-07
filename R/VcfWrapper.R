#' Wrapper for a GFF file
#'
#' Wrap a GFF2/3 file for saving and loading in the \pkg{alabaster} framework.
#'
#' @param path String containing the path to a GFF file.
#' @param compression String specifying the compression.
#' This should be one of \code{"none"}, \code{"gzip"}, \code{"bzip2"} or \code{"bgzip"}.
#' If \code{NULL}, this is inferred from the file's headers and suffix.
#' @param index String specifying the path to an index file in tabix format, or \code{NULL} if no index is available.
#' If an index is supplied, the file should be bgzip-compressed.
#' @param header_only Logical scalar specifying whether this file just contains the VCF headers without any actual data.
#' Occasionally useful for applications to perform preflight requests before downloading the full dataset.
#'
#' @details
#' The VcfWrapper class is a subclass of a \linkS4class{CompressedIndexedWrapper},
#' so all of the methods of the latter can also be used here, e.g., \code{path}, \code{index}, \code{compression}.
#' 
#' The \code{stageObject} method for VcfWrapper classes will check the GFF file by reading the first few lines 
#' and attempting to import it into a GRanges via \code{readVCF} from the \pkg{VariantAnnotation} package.
#' If an index file is supplied, it will attempt to use that index in \code{\link{headerTabix}}.
#'
#' @author Aaron Lun
#'
#' @return A VcfWrapper instance that can be used in \code{\link{stageObject}}.
#'
#' @examples
#' # Using VariantAnnotations's example file.
#' fl <- system.file("extdata", "structural.vcf", package="VariantAnnotation")
#'
#' # Creating a VcfWrapper.
#' wrapped <- VcfWrapper(fl)
#' wrapped
#'
#' # Staging the VcfWrapper.
#' dir <- tempfile()
#' library(alabaster.base)
#' info <- stageObject(wrapped, dir, "my_vcf")
#' invisible(.writeMetadata(info, dir))
#' list.files(dir, recursive=TRUE)
#'
#' # Loading it back again:
#' meta <- acquireMetadata(dir, "my_vcf/file.vcf")
#' loadObject(meta, dir)
#' 
#' @docType class
#' @aliases
#' VcfWrapper-class
#' stageObject,VcfWrapper-method
#' loadVcfWrapper
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
        handle <- TabixFile(x@path, index=x@index@path)
        headerTabix(handle)
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
#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
loadVcfWrapper <- function(meta, project) {
    load_compressed_indexed_wrapper(meta$path, meta$vcf_file, project, constructor=VcfWrapper)
}
