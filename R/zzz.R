#' @import alabaster.base
.onLoad <- function(libname, pkgname) {
    registerReadObjectFunction("bam_file", readBamFileReference)
    registerReadObjectFunction("bcf_file", readBcfFileReference)
    registerReadObjectFunction("bigbed_file", readBigBedFileReference)
    registerReadObjectFunction("bigwig_file", readBigWigFileReference)
    registerReadObjectFunction("bed_file", readBedFileReference)
    registerReadObjectFunction("gff_file", readGffFileReference)
    registerReadObjectFunction("gmt_file", readGmtFileReference)
    registerReadObjectFunction("fasta_file", readFastaFileReference)
    registerReadObjectFunction("fastq_file", readFastqFileReference)
}

.onUnload <- function(libname, pkgname) {
    registerReadObjectFunction("bam_file", NULL)
    registerReadObjectFunction("bcf_file", NULL)
    registerReadObjectFunction("bigbed_file", NULL)
    registerReadObjectFunction("bigwig_file", NULL)
    registerReadObjectFunction("bed_file", NULL)
    registerReadObjectFunction("gff_file", NULL)
    registerReadObjectFunction("gmt_file", NULL)
    registerReadObjectFunction("fasta_file", NULL)
    registerReadObjectFunction("fastq_file", NULL)
}
