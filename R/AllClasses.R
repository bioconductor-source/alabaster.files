#' @export
#' @import methods
setClass("FileReference", contains="VIRTUAL", slots=c(path="character")) 

#' @export
setClass("BamFileReference", contains="FileReference", slots=c(index="character_OR_NULL"))

#' @export
setClass("BcfFileReference", contains="FileReference", slots=c(index="character_OR_NULL"))

#' @export
setClass("BigBedFileReference", contains="FileReference")

#' @export
setClass("BigWigFileReference", contains="FileReference")

#' @export
setClass("BedFileReference", contains="FileReference", slots=c(index="character_OR_NULL"))

#' @export
setClass("GffFileReference", contains="FileReference", slots=c(index="character_OR_NULL"))

#' @export
setClass("GmtFileReference", contains="FileReference")

#' @export
setClass("FastaFileReference", contains="FileReference", slots=c(seqtype="character", faindex="character_OR_NULL", gzindex="character_OR_NULL"))

#' @export
setClass("FastqFileReference", contains="FileReference", slots=c(
    seqtype="character", 
    qualtype="character", 
    qualoffset="integer", 
    faindex="character_OR_NULL", 
    gzindex="character_OR_NULL")
)

###########################
##### OLD STUFF HERE ######
###########################

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

#' @export
setClass("FastaWrapper", contains="CompressedIndexedWrapper", slots=c(sequence.type="character", gzindex="ANY"))

#' @export
setClass("FastqWrapper", contains="CompressedIndexedWrapper", slots=c(sequence.type="character", encoding="character", gzindex="ANY"))

#' @export
setClass("FaIndexWrapper", contains="Wrapper")

#' @export
setClass("BgzipIndexWrapper", contains="Wrapper")

#' @export
setClass("VcfWrapper", contains="CompressedIndexedWrapper", slots=c(header_only="logical"))

#' @export
setClass("GffWrapper", contains="CompressedIndexedWrapper", slots=c(format="character"))

#' @export
setClass("GmtWrapper", contains="CompressedWrapper")

#' @export
setClass("BigWigWrapper", contains="Wrapper")

#' @export
setClass("BigBedWrapper", contains="Wrapper")
