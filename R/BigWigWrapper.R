#' @export
BigWigWrapper <- function(path) {
    new("BigWigWrapper", path=path)
}

#' @export
setMethod("stageObject", "BigWigWrapper", function(x, dir, path, child=FALSE) {
    magic <- readBin(x@path, what=raw(), n=4)
    if (parse_magic_number(magic) != 0x888FFC26 && parse_magic_number(rev(magic)) != 0x888FFC26) {
        stop("cannot detect BigBed magic number at the start of the file")
    }

    info <- save_wrapper(x, dir, path, fname="file.bw")
    list(
        "$schema" = "bigwig_file/v1.json",
        path = info$path,
        bigwig_file = info$inner
    )
})

#' @export
loadBigWigWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$bigwig_file, project, constructor=BigWigWrapper)
}
