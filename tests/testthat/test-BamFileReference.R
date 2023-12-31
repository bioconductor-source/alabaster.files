# Test that the BAM wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-BamFileReference.R")

library(alabaster.base)
library(S4Vectors)
fl <- system.file("extdata", "ex1.bam", package="Rsamtools", mustWork=TRUE)

test_that("BAM reference works correctly", {
    wrapped <- BamFileReference(fl)
    expect_s4_class(wrapped, "BamFileReference")
    expect_output(show(wrapped), "BamFileReference")

    tmp <- tempfile()
    saveObject(wrapped, tmp)
    roundtrip <- readObject(tmp)
    expect_s4_class(roundtrip, "BamFileReference")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
})

test_that("BAM reference with index works correctly", {
    src.index.file <- paste0(fl, ".bai")
    index.file <- tempfile(fileext=".bai")
    file.copy(src.index.file, index.file) # renaming the index to test that the staging works with non-matching index name.
    wrapped <- BamFileReference(fl, index=index.file)

    tmp <- tempfile()
    saveObject(wrapped, tmp)
    roundtrip <- readObject(tmp)
    expect_identical(file.size(roundtrip$index), file.size(index.file))
})
