# Test that the GMT wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-GmtWrapper.R")

library(alabaster.base)
library(S4Vectors)
fl <- tempfile(fileext=".gmt")
contents <- c("SET1\tthis is set 1\tgene1\tgene2\tgene3", "SET2\tthis is set 2\tgene3\tgene5\tgene4")
writeLines(contents, con=fl)

test_that("GMT wrapper works correctly", {
    wrapped <- GmtWrapper(fl)
    expect_output(show(wrapped), "GmtWrapper")
    metadata(wrapped)$foo <- "YAY"

    # Staging the GmtWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_gmt")
    invisible(.writeMetadata(info, dir))
    expect_true("my_gmt/file.gmt" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_gmt/file.gmt")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(roundtrip, "GmtWrapper")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
    expect_identical(metadata(roundtrip), list(foo="YAY"))
})

test_that("GMT wrapper works correctly with compression", {
    comp <- paste0(fl, ".gz")
    writeLines(con=gzfile(comp), contents)

    # Correctly auto-detected.
    wrapped <- GmtWrapper(comp)
    expect_identical(compression(wrapped), "gzip")

    # Goes through a roundtrip correctly.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_gmt")
    expect_true("my_gmt/file.gmt.gz" %in% list.files(dir, recursive=TRUE))
    roundtrip <- loadObject(info, dir)
    expect_identical(compression(roundtrip), "gzip")

    # Works for bgzip as well.
    wrapped <- GmtWrapper(comp, compression="bgzip")
    expect_identical(compression(wrapped), "bgzip")
})
