#' @export
#' @importClassesFrom S4Vectors Annotated
setClass("Wrapper", contains=c("Annotated", "VIRTUAL"), slots=c(path="character"))

#' @export
setClass("IndexedWrapper", contains=c("Wrapper", "VIRTUAL"), slots=c(index="ANY"))

#' @export
setClass("CompressedWrapper", contains=c("Wrapper", "VIRTUAL"), slots=c(compression="ANY"))

#' @export
setClass("CompressedIndexedWrapper", contains=c("IndexedWrapper", "CompressedWrapper", "VIRTUAL"))

#' @export
setClass("BamWrapper", contains="IndexedWrapper", slots=c(sorted="character"))

#' @export
setClass("BedWrapper", contains="CompressedIndexedWrapper")

#' @export
setClass("TabixWrapper", contains="Wrapper")

setClass("FastaWrapper", contains="CompressedIndexedWrapper")

setClass("FastqWrapper", contains="CompressedIndexedWrapper")

setClass("VcfWrapper", contains="CompressedIndexedWrapper")

setClass("GffWrapper", contains="CompressedIndexedWrapper", slots=c(format="character"))

setClass("GmtWrapper", contains="Wrapper", slots=c(compression="character"))

setClass("BigWigWrapper", contains="Wrapper")

setClass("BigBedWrapper", contains="Wrapper")

