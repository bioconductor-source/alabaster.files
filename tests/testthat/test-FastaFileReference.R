# Test that the FASTA file references are working correctly.
# library(alabaster.files); library(testthat); source("test-FastaFileReference.R")

library(alabaster.base)
library(S4Vectors)
contents <- c(">FOO\nactgcgacgagcatcacgacgactacagcag")

test_that("FASTA file reference works correctly", {
    comp <- tempfile(fileext=".fa.gz")
    writeLines(con=gzfile(comp), contents)
    wrapped <- FastaFileReference(comp)

    dir <- tempfile()
    info <- saveObject(wrapped, dir)
    roundtrip <- readObject(dir)
    expect_s4_class(roundtrip, "FastaFileReference")
    expect_identical(file.size(path(roundtrip)), file.size(comp))
})

test_that("FASTA file reference with index works correctly", {
    fl <- tempfile(fileext=".fa")
    writeLines(con=fl, contents)
    comp <- Rsamtools::bgzip(fl, dest=tempfile(fileext=".fa.bgz"))
    faindex.file <- Rsamtools::indexFa(comp, format="fa")
    gzindex.file <- sub(".fai$", ".gzi", faindex.file)

    wrapped <- FastaFileReference(comp, faindex=faindex.file, gzindex=gzindex.file)
    expect_output(show(wrapped), "faindex:")

    dir <- tempfile()
    saveObject(wrapped, dir)
    roundtrip <- readObject(dir)
    expect_identical(file.size(roundtrip$faindex), file.size(faindex.file))
    expect_identical(file.size(roundtrip$gzindex), file.size(gzindex.file))
})
