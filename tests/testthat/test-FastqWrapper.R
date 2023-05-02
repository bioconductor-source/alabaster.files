# Test that the FASTQ wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-FastqWrapper.R")

library(alabaster.base)
library(S4Vectors)
fl <- tempfile(fileext=".fastq")
contents <- c("@FOO\nactgcgacgagcatcacgacgactacagcag\n+\n1111111111111111111111111111111")
writeLines(con=fl, contents)

test_that("FASTQ wrapper works correctly", {
    wrapped <- FastqWrapper(fl, encoding="phred")
    expect_output(show(wrapped), "FastqWrapper")
    metadata(wrapped)$foo <- "YAY"

    # Staging the FastqWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_fastq")
    invisible(.writeMetadata(info, dir))
    expect_true("my_fastq/file.fastq" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_fastq/file.fastq")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(roundtrip, "FastqWrapper")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
    expect_identical(metadata(roundtrip), list(foo="YAY"))
})

test_that("FASTQ wrapper works correctly with compression", {
    comp <- paste0(fl, ".gz")
    writeLines(con=gzfile(comp), contents)

    # Correctly auto-detected.
    wrapped <- FastqWrapper(comp, encoding="solexa")
    expect_identical(compression(wrapped), "gzip")

    # Goes through a roundtrip correctly.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_fastq")
    expect_true("my_fastq/file.fastq.gz" %in% list.files(dir, recursive=TRUE))
    roundtrip <- loadObject(info, dir)
    expect_identical(compression(roundtrip), "gzip")

    # Works for bgzip as well.
    wrapped <- FastqWrapper(comp, encoding="illumina", compression="bgzip")
    expect_identical(compression(wrapped), "bgzip")
})

test_that("FASTQ wrapper with index works correctly", {
    comp <- Rsamtools::bgzip(fl)
    index.file <- Rsamtools::indexFa(comp, format="fastq")

    wrapped <- FastqWrapper(comp, encoding="phred", index=index.file)
    expect_output(show(wrapped), "index:")
    expect_s4_class(index(wrapped), "FaidxWrapper")

    # Staging the FastqWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_fastq")
    invisible(.writeMetadata(info, dir))
    expect_true("my_fastq/index/file.fastq.bgz.fai" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_fastq/file.fastq.bgz")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(index(roundtrip), "FaidxWrapper")
    expect_identical(file.size(path(index(roundtrip))), file.size(index.file))
})
