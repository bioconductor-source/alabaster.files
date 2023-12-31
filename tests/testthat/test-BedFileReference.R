# Test that the BED wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-BedFileReference.R")

library(alabaster.base)
library(S4Vectors)
contents <- c("chr1\t1\t10000", "chr2\t6\t1000", "chr2\t99\t500", "chr3\t10\t200")

test_that("BED wrapper works correctly", {
    fl <- tempfile(fileext=".bed.gz")
    writeLines(con=gzfile(fl), contents)

    wrapped <- BedFileReference(fl)
    expect_output(show(wrapped), "BedFileReference")

    dir <- tempfile()
    saveObject(wrapped, dir)
    roundtrip <- readObject(dir)

    expect_s4_class(roundtrip, "BedFileReference")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
})

test_that("BED wrapper with index works correctly", {
    fl <- tempfile(fileext=".bed")
    writeLines(con=fl, contents)

    comp <- Rsamtools::bgzip(fl)
    index.file <- Rsamtools::indexTabix(comp, format="bed")
    wrapped <- BedFileReference(comp, index=index.file)

    # Staging the BedFileReference.
    dir <- tempfile()
    saveObject(wrapped, dir)
    roundtrip <- readObject(dir)
    expect_identical(file.size(roundtrip$index), file.size(index.file))
})
