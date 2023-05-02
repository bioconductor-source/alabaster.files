# Test that the BAM wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-BamWrapper.R")

library(alabaster.base)
library(S4Vectors)
fl <- system.file("extdata", "ex1.bam", package="Rsamtools", mustWork=TRUE)

test_that("BAM wrapper works correctly", {
    wrapped <- BamWrapper(fl)
    expect_output(show(wrapped), "BamWrapper")
    metadata(wrapped)$foo <- "YAY"

    # Staging the BamWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_bam")
    invisible(.writeMetadata(info, dir))
    expect_true("my_bam/file.bam" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_bam/file.bam")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(roundtrip, "BamWrapper")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
    expect_identical(metadata(roundtrip), list(foo="YAY"))
})

test_that("BAM wrapper with index works correctly", {
    index.file <- paste0(fl, ".bai")
    wrapped <- BamWrapper(fl, index=index.file)
    expect_s4_class(index(wrapped), "BamIndexWrapper")
    expect_output(show(wrapped), "index:")

    # Staging the BamWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_bam")
    invisible(.writeMetadata(info, dir))
    expect_true("my_bam/index/file.bam.bai" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_bam/file.bam")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(index(roundtrip), "BamIndexWrapper")
    expect_identical(file.size(path(index(roundtrip))), file.size(index.file))
})

