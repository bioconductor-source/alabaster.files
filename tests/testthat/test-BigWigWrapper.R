# Test that the bigWig wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-BigWigWrapper.R")

library(alabaster.base)
library(S4Vectors)
fl <- system.file("tests", "test.bw", package = "rtracklayer")

test_that("bigWig wrapper works correctly", {
    wrapped <- BigWigWrapper(fl)
    expect_output(show(wrapped), "BigWigWrapper")
    metadata(wrapped)$foo <- "YAY"

    # Staging the BigWigWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_bw")
    invisible(.writeMetadata(info, dir))
    expect_true("my_bw/file.bw" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_bw/file.bw")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(roundtrip, "BigWigWrapper")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
    expect_identical(metadata(roundtrip), list(foo="YAY"))
})
