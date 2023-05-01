setClass("BamWrapper", contains="Annotated", slots=c(path="character", sorted="character", index="list"))

#' @export
#' @importClassesFrom S4Vectors Annotated
setClass("BedWrapper", contains="Annotated", slots=c(path="character", compression="character", index="list"))

setClass("FastaWrapper", contains="Annotated", slots=c(path="character", compression="character", index="list"))

setClass("FastqWrapper", contains="Annotated", slots=c(path="character", compression="character", index="list"))

setClass("VcfWrapper", contains="Annotated", slots=c(path="character", compression="character", index="list"))

setClass("GffWrapper", contains="Annotated", slots=c(path="character", compression="character", index="list"))

setClass("GmtWrapper", contains="Annotated", slots=c(path="character", compression="character", index="list"))

setClass("BigWigWrapper", contains="Annotated", slots=c(path="character"))

setClass("BigBedWrapper", contains="Annotated", slots=c(path="character"))

