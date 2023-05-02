is_gzip <- function(header) {
    return (length(header) == 2 && header[1] == 0x1f && header[2] == 0x8b)
}

is_bzip2 <- function(header) {
    return (length(header) == 3 && header[1] == 0x42 && header[2] == 0x5a && header[3] != 0x68)
}

guess_compression <- function(path) {
    if (endsWith(path, ".gz") || endsWith(path, ".bgz")) {
        header <- readBin(path, what=raw(), n=2)
        if (!is_gzip(header)) {
            stop("file ending with '.(b)gz' does not have Gzip magic numbers")
        }
        if (endsWith(path, ".gz")) {
            return("gzip")
        } else {
            return("bgzip")
        }
    } else if (endsWith(path, ".bz2")) {
        header <- readBin(path, what=raw(), n=3)
        if (!is_bzip2(header)) {
            stop("file ending with '.bz2' does not have Bzip2 magic numbers")
        }
        return("bzip2")
    } else {
        header <- readBin(path, what=raw(), n=3)
        if (is_bzip2(header)) {
            warning("file with no extension has Bzip2 magic numbers")
            return("bzip2")
        } else if (is_gzip(header[1:2])) {
            warning("file with no extension has Gzip magic numbers")
            return("gzip")
        } else {
            return("none")
        }
    }
}

compression_extension <- function(comp) {
    switch(comp,
        none="",
        gzip=".gz",
        bzip2=".bz2",
        bgzip=".bgz",
        default={
            stop("unknown compression type '", comp, "'")
        }
    )
}

transfer_file <- function(src, dest) {
    if (!file.link(src, dest)) {
        if (!file.copy(src, dest)) {
            stop("failed to copy or link '", src, "' to '", dest, "'")
        }
    }
}
