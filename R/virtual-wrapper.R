#' Virtual wrapper classes
#'
#' Defines some base classes for the concrete wrappers for specific file formats.
#' This provides a standard set of methods that can be applied to all Wrapper instances.
#'
#' @section Wrapper methods:
#' Any instance \code{x} of a base Wrapper class can be used with the \code{path(x)} method,
#' which returns a string containing the path to the file on the current file system.
#'
#' The Wrapper class inherits from the \linkS4class{Annotated} class,
#' so users can also get and set metadata via \code{\link{metadata}(x)}.
#'
#' @section IndexedWrapper methods:
#' The IndexedWrapper class inherits from the Wrapper class and can be used with all its methods.
#' It additionally implements the \code{index(x)} method, which returns another Wrapper object for the associated index file
#' (or \code{NULL}, if no index file exists).
#'
#' @section CompressedWrapper methods:
#' The CompressedWrapper class inherits from the Wrapper class and can be used with all its methods.
#' It additionally implements the \code{compression(x)} method, which returns a string specifying the compression strategy.
#'
#' @section CompressedIndexedWrapper methods:
#' The CompressedIndexedWrapper class inherits from both the IndexedWrapper and CompressedWrapper classes and can be used with all their methods.
#'
#' @author Aaron Lun
#' @docType class
#' @aliases
#' Wrapper-class
#' IndexedWrapper-class
#' CompressedIndexWrapper-class
#'
#' path,Wrapper-method
#' index
#' compression
#' show,CompressedIndexedWrapper-method
#'
#' @name virtual-wrapper
NULL

#' @export
#' @importFrom BiocGenerics path
setMethod("path", "Wrapper", function(object) object@path)

########################
########################

#' @export
index <- function(x) x@index

########################
########################

#' @export
compression <- function(x) x@compression

construct_compressed_indexed_wrapper <- function(path, compression, index, wrapper_class, index_constructor, ...) {
    if (is.null(compression)) {
        compression <- guess_compression(path)
    }

    x <- new(wrapper_class, path=path, compression=compression, ...)

    if (!is.null(index)) {
        if (is.character(index)) {
            index <- index_constructor(index)
        }
        x@index <- index
    }

    x
}

#' @export
#' @importFrom S4Vectors coolcat metadata
setMethod("show", "CompressedIndexedWrapper", function(object) {
    cat(class(object)[1], "object\n")
    cat("path:", object@path, "\n")
    cat("compression:", object@compression, "\n")

    if (length(object@index)) {
        cat("index:", object@index[[1]]@path, "\n")
    } else {
        cat("index: <none>\n")
    }

    coolcat("metadata(%i): %s", names(metadata(object)))
})

#' @importFrom alabaster.base .stageObject stageObject .writeMetadata .processMetadata
save_compressed_indexed_wrapper <- function(x, dir, path, fname, index_class) {
    dir.create(file.path(dir, path), showWarnings=FALSE, recursive=TRUE)

    target <- paste0(path, "/", fname, compression_extension(x@compression))
    host <- file.path(dir, target)
    transfer_file(x@path, host)

    inner_meta <- list(compression = x@compression)
    inner_meta$other_data <- .processMetadata(x, dir, path, "other") 

    index <- x@index
    if (!is.null(index)) {
        if (!is(index, index_class)) {
            stop("expected the index to be a '", index_class, "' instance")
        }
        imeta <- .stageObject(index, dir, paste0(path, "index"), child=TRUE)
        inner_meta$index <- list(resource=.writeMetadata(imeta, dir))
    }

    list(inner = inner_meta, path = target)
}

#' @importFrom alabaster.base .restoreMetadata acquireMetadata acquireFile .loadObject
load_compressed_indexed_wrapper <- function(path, inner_meta, project, constructor) {
    fpath <- acquireFile(project, path)

    index <- NULL
    if ("index" %in% names(inner_meta)) {
        imeta <- acquireMetadata(project, inner_meta$index$resource$path)
        index <- .loadObject(imeta, project)
    }

    output <- constructor(fpath, compression=inner_meta$compression, index=index)
    .restoreMetadata(output, mcol.data=NULL, meta.data=inner_meta$other_data, project)
}
