#' @export
BigBedWrapper <- function(path) {
    new("BigBedWrapper", path=path)
}

#' @export
setMethod("stageObject", "BigBedWrapper", function(x, dir, path, child=FALSE) {
    magic <- readBin(x@path, what=raw(), n=4)
    if (parse_magic_number(magic) != 0x8789F2EB && parse_magic_number(rev(magic)) != 0x8789F2EB) {
        stop("cannot detect BigBed magic number at the start of the file")
    }

    info <- save_wrapper(x, dir, path, fname="file.bb")
    list(
        "$schema" = "bigbed_file/v1.json",
        path = info$path,
        bigbed_file = info$inner
    )
})

#' @export
loadBigBedWrapper <- function(meta, project) {
    load_wrapper(meta$path, meta$bigbed_file, project, constructor=BigBedWrapper)
}
