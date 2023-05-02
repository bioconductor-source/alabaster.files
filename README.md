# Save common bioinformatics file formats

The **alabaster.files** package implements methods for processing common bioinformatics file formats (e.g., BAM, GFF, BED) in the **alabaster** framework.
This allows users to handle these files as part of larger Bioconductor objects, e.g., by embedding them inside the `metadata()` field of a `SummarizedExperiment`.
It also supports addition of metadata on each file, either directly via the wrapper object's `metadata()` method or via application-specific schemas.
Check out the [userguide](vignettes/userguide.Rmd) for more details.
