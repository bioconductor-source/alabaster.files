#' Virtual file reference class
#'
#' A virtual class for file reference objects.
#' This implements common methods for \code{\link{path}}, \code{\link{[[}} and \code{\link{$}}.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' FileReference-class
#' path,FileReference-method
#' [[,FileReference-method
#' [[<-,FileReference-method
#' $,FileReference-method
#' $<-,FileReference-method
#' show,FileReference-method
#'
#' @name FileReference
NULL

#' @export
#' @importFrom BiocGenerics path
setMethod("path", "FileReference", function(object, ...) object@path)

#' @export 
setMethod("[[", "FileReference", function(x, i) slot(x, i))

#' @export 
setMethod("[[<-", "FileReference", function(x, i, value) {
    slot(x, i) <- value
    x 
})

#' @export 
setMethod("$", "FileReference", function(x, name) slot(x, name))

#' @export 
setMethod("$<-", "FileReference", function(x, name, value) {
    slot(x, name) <- value
    x 
})

#' @export
setMethod("show", "FileReference", function(object) {
    cat(class(object)[1], "object\n")
    cat("path:", path(object), "\n")
    for (x in setdiff(slotNames(object), "path")) {
        val <- slot(object, x)
        if (is.null(val)) {
            cat(x, ": NULL\n", sep="")
        } else {
            cat(x, ": ", slot(object, x), "\n", sep="")
        }
    }
})
