# Test that the VCF wrappers are working correctly.
# library(alabaster.files); library(testthat); source("test-VcfWrapper.R")

library(alabaster.base)
library(S4Vectors)
src <- system.file("extdata", "structural.vcf", package="VariantAnnotation")
fl <- tempfile(fileext=".vcf")
file.copy(src, fl)

test_that("VCF wrapper works correctly", {
    wrapped <- VcfWrapper(fl)
    expect_output(show(wrapped), "VcfWrapper")
    metadata(wrapped)$foo <- "YAY"

    # Staging the VcfWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_vcf")
    invisible(.writeMetadata(info, dir))
    expect_true("my_vcf/file.vcf" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_vcf/file.vcf")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(roundtrip, "VcfWrapper")
    expect_identical(file.size(path(roundtrip)), file.size(fl))
    expect_false(roundtrip@header_only)
    expect_identical(metadata(roundtrip), list(foo="YAY"))
})

test_that("VCF wrapper works correctly with compression", {
    comp <- paste0(fl, ".gz")
    writeLines(con=gzfile(comp), readLines(fl))

    # Correctly auto-detected.
    wrapped <- VcfWrapper(comp)
    expect_identical(compression(wrapped), "gzip")

    # Goes through a roundtrip correctly.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_vcf")
    expect_true("my_vcf/file.vcf.gz" %in% list.files(dir, recursive=TRUE))
    roundtrip <- loadObject(info, dir)
    expect_identical(compression(roundtrip), "gzip")

    # Works for bgzip as well.
    wrapped <- VcfWrapper(comp, compression="bgzip")
    expect_identical(compression(wrapped), "bgzip")
})

test_that("VCF wrapper with index works correctly", {
    comp <- Rsamtools::bgzip(fl)
    index.file <- Rsamtools::indexTabix(comp, format="vcf")

    wrapped <- VcfWrapper(comp, index=index.file)
    expect_output(show(wrapped), "index:")
    expect_s4_class(index(wrapped), "TabixIndexWrapper")

    # Staging the VcfWrapper.
    dir <- tempfile()
    info <- stageObject(wrapped, dir, "my_vcf")
    invisible(.writeMetadata(info, dir))
    expect_true("my_vcf/index/file.vcf.bgz.tbi" %in% list.files(dir, recursive=TRUE))

    # Loading it back again:
    meta <- acquireMetadata(dir, "my_vcf/file.vcf.bgz")
    roundtrip <- loadObject(meta, dir)
    expect_s4_class(index(roundtrip), "TabixIndexWrapper")
    expect_identical(file.size(path(index(roundtrip))), file.size(index.file))
})
