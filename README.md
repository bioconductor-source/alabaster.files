# Save common bioinformatics file formats

|Environment|Status|
|---|---|
|[BioC-release](https://bioconductor.org/packages/release/bioc/html/alabaster.files.html)|[![Release OK](https://bioconductor.org/shields/build/release/bioc/alabaster.files.svg)](http://bioconductor.org/checkResults/release/bioc-LATEST/alabaster.files/)|
|[BioC-devel](https://bioconductor.org/packages/devel/bioc/html/alabaster.files.html)|[![Devel OK](https://bioconductor.org/shields/build/devel/bioc/alabaster.files.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/alabaster.files/)|

The **alabaster.files** package implements methods for processing common bioinformatics file formats (e.g., BAM, GFF, BED) in the **alabaster** framework.
This allows users to store these files as part of larger Bioconductor objects, e.g., by embedding them inside the `metadata()` field of a `SummarizedExperiment`.
To get started, install the package and its dependencies from [Bioconductor](https://bioconductor.org/packages/alabaster.files):

```r
# install.packages("BiocManager")
BiocManager::install("alabaster.files")
```

We can then assemble complex objects containing various files:

```r
library(alabaster.files)
library(S4Vectors)
df <- DataFrame(Sample=LETTERS[1:4])

# Adding a column of assorted wrapper files:
df$File <- list(
    BamFileReference(system.file("extdata", "ex1.bam", package="Rsamtools")),
    BigWigFileReference(system.file("tests", "test.bw", package = "rtracklayer")),
    BigBedFileReference(system.file("tests", "test.bb", package = "rtracklayer")),
    BcfFileReference(system.file("extdata", "ex1.bcf.gz", package = "Rsamtools"))
)

# Saving it all to the staging directory:
dir <- tempfile()
saveObject(df, dir)
```

When we load the parent `DataFrame` back into the R session, we have references to copies of the files in the staging directory.
Users can inspect the `path` from each `*Reference` for further analysis.

```r
roundtrip <- readObject(dir)
roundtrip$File
## [[1]]
## BamFileReference object
## path: /tmp/RtmpdTiBzJ/file136647b568cb/other_columns/1/other_contents/0/file.bam
## index: NULL
## 
## [[2]]
## BigWigFileReference object
## path: /tmp/RtmpdTiBzJ/file136647b568cb/other_columns/1/other_contents/1/file.bw
## 
## [[3]]
## BigBedFileReference object
## path: /tmp/RtmpdTiBzJ/file136647b568cb/other_columns/1/other_contents/2/file.bb
## 
## [[4]]
## BcfFileReference object
## path: /tmp/RtmpdTiBzJ/file136647b568cb/other_columns/1/other_contents/3/file.bcf
## index: NULL
```
