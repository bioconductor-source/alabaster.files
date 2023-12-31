# Test that the FASTQ file references are working correctly.
# library(alabaster.files); library(testthat); source("test-FastqFileReference.R")

library(alabaster.base)
library(S4Vectors)
contents <- c("@FOO\nactgcgacgagcatcacgacgactacagcag\n+\n1111111111111111111111111111111")

test_that("FASTQ file reference works correctly", {
    comp <- tempfile(fileext=".fq.gz")
    writeLines(con=gzfile(comp), contents)
    wrapped <- FastqFileReference(comp)

    dir <- tempfile()
    saveObject(wrapped, dir)
    roundtrip <- readObject(dir)
    expect_identical(file.size(path(roundtrip)), file.size(comp))
})

test_that("FASTQ file reference with index works correctly", {
    fl <- tempfile(fileext=".fq")
    writeLines(con=fl, contents)
    comp <- Rsamtools::bgzip(fl, dest=tempfile(fileext=".bgz"))
    faindex.file <- Rsamtools::indexFa(comp)
    gzindex.file <- sub(".fai$", ".gzi", faindex.file)

    wrapped <- FastqFileReference(comp, faindex=faindex.file, gzindex=gzindex.file)
    expect_output(show(wrapped), "faindex:")

    dir <- tempfile()
    saveObject(wrapped, dir)
    roundtrip <- readObject(dir)
    expect_identical(file.size(roundtrip$faindex), file.size(faindex.file))
    expect_identical(file.size(roundtrip$gzindex), file.size(gzindex.file))
})
