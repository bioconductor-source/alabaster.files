# Test that the bigBed wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-BigBedWrapper.R")

library(alabaster.base)
library(S4Vectors)
fl <- system.file("tests", "test.bb", package = "rtracklayer")

test_that("BigBed wrapper works correctly", {
    wrapped <- BigBedWrapper(fl)
    expect_output(show(wrapped), "BigBedWrapper")
    metadata(wrapped)$foo <- "YAY"

    # Staging the BigBedWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_bb")
    invisible(.writeMetadata(info, dir))
    expect_true("my_bb/file.bb" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_bb/file.bb")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(roundtrip, "BigBedWrapper")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
    expect_identical(metadata(roundtrip), list(foo="YAY"))
})
