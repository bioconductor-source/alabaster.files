# Test that the GMT file references are working correctly.
# library(alabaster.files); library(testthat); source("test-GmtFileReference.R")

library(alabaster.base)
library(S4Vectors)
fl <- tempfile(fileext=".gmt.gz")
contents <- c("SET1\tthis is set 1\tgene1\tgene2\tgene3", "SET2\tthis is set 2\tgene3\tgene5\tgene4")
writeLines(contents, con=gzfile(fl))

test_that("GMT file reference works correctly", {
    wrapped <- GmtFileReference(fl)
    expect_output(show(wrapped), "GmtFileReference")

    dir <- tempfile()
    saveObject(wrapped, dir)
    roundtrip <- readObject(dir)
    expect_s4_class(roundtrip, "GmtFileReference")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
})
