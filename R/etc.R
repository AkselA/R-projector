
#' Read DESCRIPTION
#' 
#' Read the DESCRIPTION file of a given package
#' 
#' @param projname path to a package source, by default "." (current directory)
#' @param quiet logical; if FALSE (default) results will be printed to console
#' 
#' @seealso \code{\link{desc_write}}, \code{\link{increment_version}}
#' 
#' @export

desc_read <- function(projname=".", quiet=FALSE) {
    if (!dir.exists(projname)) {
        stop(paste("Could not find directory", sQuote(projname)))
    }
    fpath <- file.path(projname, "DESCRIPTION")
    if (!file.exists(fpath)) {
        stop(paste("Could not find a DESCRIPTION file in the given directory"))
    }
    d <- read.dcf(fpath)[1,]
    d <- gsub(" *\n *", " ", d)
    if (quiet) {
        d
	} else {
        cat(readLines(fpath), sep="\n")
        invisible(d)
	}
}


#' Write DESCRIPTION
#' 
#' Write a DESCRIPTION file for a given package
#' 
#' @param projname path to a package source, by default "." (current directory)
#' @param descv a named character vector giving key-value pairs
#' 
#' @seealso \code{\link{desc_read}}, \code{\link{increment_version}}
#' 
#' @export

desc_write <- function(projname=".", descv) {
    if (!dir.exists(projname)) {
        stop(paste("Could not find", projname, "in current directory"))
    }
    desc <- strwrap(paste0(names(descv), ": ", descv), 80, exdent=4)
    p <- normalizePath(file.path(projname, "DESCRIPTION"))
    cat(desc, sep="\n", file=p)
}


#' Increment version
#' 
#' Increment version number in the DESCRIPTION file of a given package
#' 
#' @param projname path to a package source, by default "." (current directory)
#' @param comp which component to increment. By default "patch"
#' @param dev logical; is it a development version?
#' @param quiet logical; if FALSE (default) results will be printed to console
#' 
#' @details
#' Writes version numbers of the format "maj.min.patch.dev", but will also read
#' formats with hyphen seperated components, like "maj.min-patch". If \code{dev}
#' is \code{TRUE}, a version number of the form "maj.min.patch.9000" is used,
#' otherwise "maj.min.patch".
#' Other than version number, date, imports and R version number is updated as well.
#' 
#' @seealso \code{\link{desc_read}}, \code{\link{desc_write}}
#' 
#' @export

increment_version <- function(projname=".", comp=c("patch", "minor", "major"),
  dev=FALSE, quiet=FALSE) {
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
    dep <- strsplit(d["Depends"], ", ?")[[1]]
    dep[grep("^R ", dep)] <- paste0("R (>= ", getRversion(), ")")
    d["Depends"] <- paste(dep, collapse=", ")
    nam <- readLines(file.path(projname, "NAMESPACE"))
    imp <- grep("^import", nam, value=TRUE)
    imp <- sapply(strsplit(imp, "[\\(\\),]"), "[", 2)
    d["Imports"] <- paste(unique(imp), collapse=", ")
    
    if (quiet) {
        desc_write(projname, d)
    } else {
	    mat <- as.matrix(d[c("Version", "Date", "Depends", "Imports")])
	    colnames(mat) <- ""
	    print(mat, quote=FALSE)
	    invisible(desc_write(projname, d))
    }
}

#' Function parameters
#' 
#' Return the parameters of a function, formatted for Roxygen2 use
#' 
#' @param fun name of function
#' @param to.clipboard logical; should results be copied to the clipboard?
#' 
#' @details
#' The parameters are returned as "\code{@param <par>}" pairs, one per line, as
#' used by Roxygen2. The results can be either printed to console or copied to the
#' clipboard, in either case a character vector will be returned invisibly as well.
#' 
#' @seealso \code{\link{roxdoc}}
#' 
#' @examples
#' params(params, FALSE)
#' params("params", FALSE)
#' 
#' @export

params <- function(fun, to.clipboard=TRUE) {
    ps <- paste0("@param ", formalArgs(args(fun)))
    if (to.clipboard) {
        cbpaste <- pipe("pbcopy", "w")                           
        writeLines(ps, con=cbpaste)
        close(cbpaste)
    } else {
        cat(ps, sep="\n")
    }
        invisible(ps)
}

#' Documentation template
#' 
#' Create a function documentation template
#' 
#' @param fun function or name of function
#' @param to.clipboard logical; should results be copied to the clipboard?
#' 
#' @details
#' The function arguments are returned as with \code{\link{params}}. 
#' In addition other commonly used Roxygen2 tags are included.
#' 
#' @return
#' Either to the console, or clipboard, a function documentation template, as
#' appropriate for Roxygen2 use, is returned. Tags included are \code{@param}
#' (with argument names), \code{@details}, \code{@return}, \code{@seealso},
#' \code{@examples} and \code{@export}.
#' 
#' @seealso \code{\link{params}}
#' 
#' @examples
#' roxdoc(roxdoc, FALSE)
#' roxdoc("roxdoc", FALSE)
#' 
#' @export

roxdoc <- function(fun, to.clipboard=TRUE) {
    ps <- paste0("@param ", formalArgs(args(fun)))
    sc <- c("", ps, "",
            "@details\n",
            "@return\n",
            "@seealso \\code{\\link{}}\n",
            "@examples\n",
            "@export\n")
    if (to.clipboard) {
        cbpaste <- pipe("pbcopy", "w")                           
        writeLines(sc, con=cbpaste)
        close(cbpaste)
    } else {
        cat(sc, sep="\n")
    }
        invisible(sc)
}


#' Write .Rbuildignore
#' 
#' Write an .Rbuildignore file for a given package
#' 
#' @param projname path to a package source, by default "." (current directory)
#' @param add logical; write new, or add to the existing file?
#' @param pat a character vector of regex patterns
#' @param verbose logical; print .Rbuildignore contents
#' 
#' @export

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


#' R manuals
#' 
#' Browse offline R manuals
#' 
#' @param topic which help topic to browse. By default "extensions" ("R-exts.html")
#' 
#' @details
#' The AQUA browser will be used if AQUA is used as the GUI, otherwise
#' \code{getOption("browser")} is used.
#' 
#' @export

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
        browser <- get("aqua.browser", envir=as.environment("tools:RGUI"))
    } else {
	    browser <- getOption("browser")
    }
    browseURL(paste0("http://127.0.0.1:", port, "/doc/manual/", man),
      browser=browser)
}

