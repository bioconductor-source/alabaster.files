# Test that the bigWig wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-BigWigFileReference.R")

library(alabaster.base)
library(S4Vectors)
fl <- system.file("tests", "test.bw", package = "rtracklayer")

test_that("BigWig wrapper works correctly", {
    wrapped <- BigWigFileReference(fl)
    expect_s4_class(wrapped, "BigWigFileReference")
    expect_output(show(wrapped), "BigWigFileReference")

    # Staging the BigWigFileReference.
    dir <- tempfile()
    info <- saveObject(wrapped, dir)
    roundtrip <- readObject(dir)
    expect_s4_class(roundtrip, "BigWigFileReference")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
})
