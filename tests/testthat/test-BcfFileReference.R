# Test that the BCF references are working correctly.
# library(alabaster.files); library(testthat); source("test-BcfFileReference.R")

library(alabaster.base)
library(S4Vectors)
fl <- system.file("extdata", "ex1.bcf.gz", package="Rsamtools", mustWork=TRUE)

test_that("BCF reference works correctly", {
    wrapped <- BcfFileReference(fl)
    expect_s4_class(wrapped, "BcfFileReference")
    expect_output(show(wrapped), "BcfFileReference")

    tmp <- tempfile()
    saveObject(wrapped, tmp)
    roundtrip <- readObject(tmp)
    expect_s4_class(roundtrip, "BcfFileReference")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
})

test_that("BCF reference with index works correctly", {
    src.index.file <- paste0(fl, ".csi")
    index.file <- tempfile(fileext=".csi")
    file.copy(src.index.file, index.file) # renaming the index to test that the staging works with non-matching index name.
    wrapped <- BcfFileReference(fl, index=index.file)

    tmp <- tempfile()
    saveObject(wrapped, tmp)
    roundtrip <- readObject(tmp)
    expect_identical(file.size(roundtrip$index), file.size(index.file))
})
