#' @export
#' @import methods
#' @importClassesFrom S4Vectors Annotated
setClass("Wrapper", contains=c("Annotated", "VIRTUAL"), slots=c(path="character"))

#' @export
setClass("IndexedWrapper", contains=c("Wrapper", "VIRTUAL"), slots=c(index="ANY"))

#' @export
setClass("CompressedWrapper", contains=c("Wrapper", "VIRTUAL"), slots=c(compression="ANY"))

#' @export
setClass("CompressedIndexedWrapper", contains=c("IndexedWrapper", "CompressedWrapper", "VIRTUAL"))

#' @export
setClass("BamWrapper", contains="IndexedWrapper")

#' @export
setClass("BamIndexWrapper", contains="Wrapper")

#' @export
setClass("BedWrapper", contains="CompressedIndexedWrapper")

#' @export
setClass("TabixIndexWrapper", contains="Wrapper")

setClass("FastaWrapper", contains="CompressedIndexedWrapper", slots=c(sequence.type="character", gzindex="ANY"))

setClass("FastqWrapper", contains="CompressedIndexedWrapper", slots=c(sequence.type="character", encoding="character", gzindex="ANY"))

#' @export
setClass("FaIndexWrapper", contains="Wrapper")

#' @export
setClass("BgzipIndexWrapper", contains="Wrapper")

setClass("VcfWrapper", contains="CompressedIndexedWrapper")

setClass("GffWrapper", contains="CompressedIndexedWrapper", slots=c(format="character"))

#' @export
setClass("GmtWrapper", contains="CompressedWrapper")

setClass("BigWigWrapper", contains="Wrapper")

setClass("BigBedWrapper", contains="Wrapper")
