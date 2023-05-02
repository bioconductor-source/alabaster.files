# Test that the GFF wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-GffWrapper.R")

library(alabaster.base)
library(S4Vectors)
src <- system.file("tests", "genes.gff3", package = "rtracklayer")
fl <- tempfile(fileext=".gff3")
file.copy(src, fl)

test_that("GFF wrapper works correctly", {
    wrapped <- GffWrapper(fl)
    expect_output(show(wrapped), "GffWrapper")
    metadata(wrapped)$foo <- "YAY"

    # Staging the GffWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_gff")
    invisible(.writeMetadata(info, dir))
    expect_true("my_gff/file.gff3" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_gff/file.gff3")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(roundtrip, "GffWrapper")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
    expect_identical(roundtrip@format, "GFF3")
    expect_identical(metadata(roundtrip), list(foo="YAY"))
})

test_that("GFF wrapper works correctly with compression", {
    comp <- paste0(fl, ".gz")
    writeLines(con=gzfile(comp), readLines(fl))

    # Correctly auto-detected.
    wrapped <- GffWrapper(comp)
    expect_identical(compression(wrapped), "gzip")

    # Goes through a roundtrip correctly.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_gff")
    expect_true("my_gff/file.gff3.gz" %in% list.files(dir, recursive=TRUE))
    roundtrip <- loadObject(info, dir)
    expect_identical(compression(roundtrip), "gzip")

    # Works for bgzip as well.
    wrapped <- GffWrapper(comp, compression="bgzip")
    expect_identical(compression(wrapped), "bgzip")
})

test_that("GFF wrapper with index works correctly", {
    comp <- Rsamtools::bgzip(fl)
    index.file <- Rsamtools::indexTabix(comp, format="gff")

    wrapped <- GffWrapper(comp, index=index.file)
    expect_output(show(wrapped), "index:")
    expect_s4_class(index(wrapped), "TabixWrapper")

    # Staging the GffWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_gff")
    invisible(.writeMetadata(info, dir))
    expect_true("my_gff/index/file.gff3.bgz.tbi" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_gff/file.gff3.bgz")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(index(roundtrip), "TabixWrapper")
    expect_identical(file.size(path(index(roundtrip))), file.size(index.file))
})
