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

read_first_few_lines <- function(x, compression, n=10, comment="") {
    handle <- switch(compression,
        none=file(x, open="rb"),
        bzip2=bzfile(x, open="rb"),
        gzip=gzfile(x, open="rb"),
        bgzip=gzfile(x, open="rb")
    )
    on.exit(close(handle))

    if (comment == "") {
        return(readLines(handle, n=n))
    }

    # Keep on going until we collect enough non-comment lines. The comments are
    # still returned, but we want to make sure that we have at least some non-comment lines.
    collected <- list()
    non.comments <- 0L
    repeat {
        current <- readLines(handle, n=n)
        collected[[length(collected) + 1]] <- current
        non.comments <- non.comments + sum(!startsWith(current, comment))
        if (length(current) < n || non.comments >= n) {
            break;
        }
    }

    unlist(collected)
}

parse_magic_number <- function(bytes) {
    sum(as.double(bytes) * 256^(seq_along(bytes)-1))
}

transfer_index_file <- function(src, dest, extensions, format) {
    for (suf in extensions) {
        suf <- paste0(".", suf)
        if (endsWith(src, suf)) {
            transfer_file(src, paste0(dest, suf))
            return(NULL)
        }
    }
    stop("cannot determine ", format, " index type from its file extension")
}

choose_available_index <- function(path, extensions) {
    index <- NULL
    for (suffix in extensions) {
        ipath <- paste0(path, ".", suffix)
        if (file.exists(ipath)) {
            index <- ipath
            break
        }
    }
    index
}
