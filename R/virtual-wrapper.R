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

construct_with_index <- function(x, index, index_constructor) {
    if (!is.null(index)) {
        if (is.character(index)) {
            index <- index_constructor(index)
        }
        x@index <- index
    }
    x
}

show_with_index <- function(x) {
    if (length(x@index)) {
        cat("index:", x@index[[1]]@path, "\n")
    } else {
        cat("index: <none>\n")
    }
}

#' @importFrom alabaster.base .stageObject .writeMetadata
stage_with_index <- function(x, dir, path, inner_meta, index_class) {
    index <- x@index
    if (!is.null(index)) {
        if (!is(index, index_class)) {
            stop("expected the index to be a '", index_class, "' instance")
        }
        imeta <- .stageObject(index, dir, paste0(path, "index"), child=TRUE)
        inner_meta$index <- list(resource=.writeMetadata(imeta, dir))
    }
    inner_meta
}

#' @importFrom alabaster.base acquireMetadata .loadObject
load_with_index <- function(inner_meta, project) {
    index <- NULL
    if ("index" %in% names(inner_meta)) {
        imeta <- acquireMetadata(project, inner_meta$index$resource$path)
        index <- .loadObject(imeta, project)
    }
    index
}

construct_indexed_wrapper <- function(path, index, wrapper_class, index_constructor, ...) {
    x <- new(wrapper_class, path=path, ...)
    construct_with_index(x, index, index_constructor=index_constructor)
}

#' @export
#' @importFrom S4Vectors coolcat metadata
setMethod("show", "IndexedWrapper", function(object) {
    cat(class(object)[1], "object\n")
    cat("path:", object@path, "\n")
    show_with_index(object)
    coolcat("metadata(%i): %s", names(metadata(object)))
})

#' @importFrom alabaster.base .processMetadata
save_indexed_wrapper <- function(x, dir, path, fname, index_class) {
    dir.create(file.path(dir, path), showWarnings=FALSE, recursive=TRUE)

    target <- paste0(path, "/", fname)
    host <- file.path(dir, target)
    transfer_file(x@path, host)

    inner_meta <- list()
    inner_meta$other_data <- .processMetadata(x, dir, path, "other") 
    inner_meta <- stage_with_index(x, dir, path, inner_meta=inner_meta, index_class=index_class)
    names(inner_meta) <- as.character(names(inner_meta)) # force object-ness.

    list(inner = inner_meta, path = target)
}

#' @importFrom alabaster.base .restoreMetadata acquireFile
load_indexed_wrapper <- function(path, inner_meta, project, constructor) {
    fpath <- acquireFile(project, path)
    index <- load_with_index(inner_meta, project)
    output <- constructor(fpath, compression=inner_meta$compression, index=index)
    .restoreMetadata(output, mcol.data=NULL, meta.data=inner_meta$other_data, project)
}

########################
########################

#' @export
compression <- function(x) x@compression

construct_compressed_indexed_wrapper <- function(path, compression, index, wrapper_class, index_constructor, ...) {
    if (is.null(compression)) {
        compression <- guess_compression(path)
    }
    x <- new(wrapper_class, path=path, compression=compression, ...)
    construct_with_index(x, index, index_constructor=index_constructor)
}

#' @export
#' @importFrom S4Vectors coolcat metadata
setMethod("show", "CompressedIndexedWrapper", function(object) {
    cat(class(object)[1], "object\n")
    cat("path:", object@path, "\n")
    cat("compression:", object@compression, "\n")
    show_with_index(object)
    coolcat("metadata(%i): %s", names(metadata(object)))
})

#' @importFrom alabaster.base .processMetadata
save_compressed_indexed_wrapper <- function(x, dir, path, fname, index_class) {
    dir.create(file.path(dir, path), showWarnings=FALSE, recursive=TRUE)

    target <- paste0(path, "/", fname, compression_extension(x@compression))
    host <- file.path(dir, target)
    transfer_file(x@path, host)

    inner_meta <- list(compression = x@compression)
    inner_meta$other_data <- .processMetadata(x, dir, path, "other") 
    inner_meta <- stage_with_index(x, dir, path, inner_meta=inner_meta, index_class=index_class)

    list(inner = inner_meta, path = target)
}

#' @importFrom alabaster.base .restoreMetadata acquireFile 
load_compressed_indexed_wrapper <- function(path, inner_meta, project, constructor) {
    fpath <- acquireFile(project, path)
    index <- load_with_index(inner_meta, project)
    output <- constructor(fpath, compression=inner_meta$compression, index=index)
    .restoreMetadata(output, mcol.data=NULL, meta.data=inner_meta$other_data, project)
}
