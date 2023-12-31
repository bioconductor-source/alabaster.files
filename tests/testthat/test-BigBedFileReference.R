# Test that the bigBed wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-BigBedFileReference.R")

library(alabaster.base)
library(S4Vectors)
fl <- system.file("tests", "test.bb", package = "rtracklayer")

test_that("BigBed wrapper works correctly", {
    wrapped <- BigBedFileReference(fl)
    expect_s4_class(wrapped, "BigBedFileReference")
    expect_output(show(wrapped), "BigBedFileReference")

    # Staging the BigBedFileReference.
    dir <- tempfile()
    info <- saveObject(wrapped, dir)
    roundtrip <- readObject(dir)
    expect_s4_class(roundtrip, "BigBedFileReference")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
})
