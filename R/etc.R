
# Read DESCRIPTION file

desc_read <- function(projname=".", quiet=FALSE) {
    if (!dir.exists(projname)) {
        stop(paste("Could not find", projname, "in current directory"))
    }
    d <- scan(file.path(projname, "DESCRIPTION"), "", sep="\n", quiet=quiet)
    d <- split(d, cumsum(grepl("^[^#: -][^: ]+:", d)))
    d <- gsub(" +|\t", " ", sapply(d, paste, collapse="\n"))
    d <- gsub(" *\n *", " ", sapply(d, paste, collapse="\n"))
    names(d) <- sub(": .*", "", d)
    d <- sub("^[^:]+: *", "", d)
    d <- sub("^([^:]+): *.*", "\\1", d)
    if (!quiet) {
	    mat <- as.matrix(d)
	    colnames(mat) <- ""
	    print(mat, quote=FALSE)
        invisible(d)
	} else {
		d
	}
}

desc_write <- function(projname=".", descv) {
    if (!dir.exists(projname)) {
        stop(paste("Could not find", projname, "in current directory"))
    }
    desc <- strwrap(paste0(names(descv), ": ", descv), 80, exdent=4)
    p <- normalizePath(file.path(projname, "DESCRIPTION"))
    cat(desc, sep="\n", file=p)
}

n <- c("Package", "Version", "Date", "Title", "Authors@R", 
"Description", "Depends", "Imports", "Suggests", "License", 
"URL", "LazyData", "Encoding")
# nt <- n %in% names(d)

increment_version <- function(projname=".", comp=c("patch", "minor", "major"),
  dev=FALSE) {
  	d <- desc_read(projname, quiet=TRUE)
	g <- as.numeric(strsplit(d["Version"], "[.-]")[[1]])
    length(g) <- 3
    g[is.na(g)] <- 0
    comp="patch"
    comp <- match.arg(comp)
    new <- switch(comp,
      patch={
      	g[3] <- g[3]+1
      },
      minor={
      	g[2] <- g[2]+1
      	g[3] <- 0
      },
      major={
      	g[1] <- g[1]+1
      	g[2] <- 0
      	g[3] <- 0
      })
    if (dev) {
    	g <- c(g, 9000)
    }
    d["Version"] <- paste(g, collapse=".")
    d["Date"] <- as.character(Sys.Date())
    d["Depends"] <- paste0("R (>= ", getRversion(), ")")
    mat <- as.matrix(d[c("Version", "Date", "Depends")])
    colnames(mat) <- ""
    print(mat, quote=FALSE)
    desc_write(projname, d)
}


# get a list of function arguments ready for Roxygen2 use
params <- function(fun, to.clipboard=TRUE) {
    ps <- paste0("@param ", names(formals(fun)))
    if (to.clipboard) {
        cbpaste <- pipe("pbcopy", "w")                           
        writeLines(ps, con=cbpaste)
        close(cbpaste)
        invisible(ps)
    } else {
        cat(ps, sep="\n")
        invisible(ps)
    }
}

# Write .Rbuildignore file
buildignore <- function(projname=".", add=FALSE, 
  pat=c("^commit\\.command$", "\\.Rproj$", "^__.*", "^\\.DS_Store$"),
  verbose=TRUE) {
    if (projname == ".") {
		odir <- getwd()
		projname <- basename(odir)
		on.exit(setwd(odir))
		setwd(dirname(odir))
	}
    bignore.path <- file.path(projname, ".Rbuildignore") 
    if (!file.exists(bignore.path)) {
        file.create(bignore.path)
    }
    if (add) {
        pat <- union(scan(bignore.path, "", sep="\n"), pat)
    }
    if (verbose) {
        message(".Rbuildignore:")
        cat(pat, sep="\n")
    }
    cat(pat, file=bignore.path, sep="\n")
}

# Browse the various R manuals
r_manual <- function(topic=c("extensions", "administration", "installation", "data",
  "I/O", "FAQ", "introduction", "internals", "language definition")) {
  	topic <- match.arg(topic)
  	man <- switch(topic,
  	  "extensions"="R-exts.html",
  	  "administration"=,
  	  "installation"="R-admin.html",
  	  "data"=,
  	  "I/O"="R-data.html",
  	  "FAQ"="R-FAQ.html",
  	  "introduction"="R-intro.html",
  	  "internals"="R-ints.html",
  	  "language definition"="R-lang.html"
  	)
	port <- tools::startDynamicHelp(NA)
    if (.Platform$GUI == "AQUA") {
        browser <- get("aqua.browser", envir = as.environment("tools:RGUI"))
    } else {
	    browser <- getOption("browser")
    }
    browseURL(paste0("http://127.0.0.1:", port, "/doc/manual/", man),
      browser=browser)
}

