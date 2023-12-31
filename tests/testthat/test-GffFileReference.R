# Test that the GFF file references are working correctly.
# library(alabaster.files); library(testthat); source("test-GffFileReference.R")

library(alabaster.base)
library(S4Vectors)
src <- system.file("tests", "genes.gff3", package = "rtracklayer")

test_that("GFF file reference works correctly", {
    fl <- tempfile(fileext=".gff3.gz")
    writeLines(con=gzfile(fl), readLines(src))
    wrapped <- GffFileReference(fl)
    expect_output(show(wrapped), "GffFileReference")

    dir <- tempfile()
    saveObject(wrapped, dir)
    roundtrip <- readObject(dir)
    expect_s4_class(roundtrip, "GffFileReference")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
})

test_that("GFF file reference with index works correctly", {
    fl <- tempfile(fileext=".gff3")
    file.copy(src, fl)
    comp <- Rsamtools::bgzip(fl)
    index.file <- Rsamtools::indexTabix(comp, format="gff")
    wrapped <- GffFileReference(comp, index=index.file)

    dir <- tempfile()
    saveObject(wrapped, dir)
    roundtrip <- readObject(dir)
    expect_identical(file.size(roundtrip$index), file.size(index.file))
})
