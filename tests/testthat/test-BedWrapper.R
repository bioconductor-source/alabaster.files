# Test that the BED wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-BedWrapper.R")

library(alabaster.base)
library(S4Vectors)
fl <- tempfile(fileext=".bed")
contents <- c("chr1\t1\t10000", "chr2\t6\t1000", "chr2\t99\t500", "chr3\t10\t200")
writeLines(con=fl, contents)

test_that("BED wrapper works correctly", {
    wrapped <- BedWrapper(fl)
    expect_output(show(wrapped), "BedWrapper")
    metadata(wrapped)$foo <- "YAY"

    # Staging the BedWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_bed")
    invisible(.writeMetadata(info, dir))
    expect_true("my_bed/file.bed" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_bed/file.bed")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(roundtrip, "BedWrapper")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
    expect_identical(metadata(roundtrip), list(foo="YAY"))
})

test_that("BED wrapper works correctly with compression", {
    comp <- paste0(fl, ".gz")
    writeLines(con=gzfile(comp), contents)

    # Correctly auto-detected.
    wrapped <- BedWrapper(comp)
    expect_identical(compression(wrapped), "gzip")

    # Goes through a roundtrip correctly.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_bed")
    expect_true("my_bed/file.bed.gz" %in% list.files(dir, recursive=TRUE))
    roundtrip <- loadObject(info, dir)
    expect_identical(compression(roundtrip), "gzip")

    # Works for bgzip as well.
    wrapped <- BedWrapper(comp, compression="bgzip")
    expect_identical(compression(wrapped), "bgzip")
})

test_that("BED wrapper with index works correctly", {
    comp <- Rsamtools::bgzip(fl)
    index.file <- Rsamtools::indexTabix(comp, format="bed")

    wrapped <- BedWrapper(comp, index=index.file)
    expect_output(show(wrapped), "index:")
    expect_s4_class(index(wrapped), "TabixIndexWrapper")

    # Staging the BedWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_bed")
    invisible(.writeMetadata(info, dir))
    expect_true("my_bed/index/file.bed.bgz.tbi" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_bed/file.bed.bgz")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(index(roundtrip), "TabixIndexWrapper")
    expect_identical(file.size(path(index(roundtrip))), file.size(index.file))
})
