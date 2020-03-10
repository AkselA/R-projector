
# Read DESCRIPTION file
descr <- function(projname, quiet=FALSE) {
    if (!dir.exists(projname)) {
        stop(paste("Could not find", projname, "in current directory"))
    }
    d <- scan(file.path(projname, "DESCRIPTION"), "", sep="\n", quiet=quiet)
    d <- split(d, cumsum(grepl("^[^#: -][^: ]+:", d)))
    d <- gsub(" +|\t", " ", sapply(d, paste, collapse="\n"))
    d <- gsub(" *\n *", "\n", sapply(d, paste, collapse="\n"))
    names(d) <- sub(": .*", "", d)
    d <- sub("^[^:]+: *", "", d)
    sub("^([^:]+): *.*", "\\1", d)
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
buildignore <- function(projname, add=FALSE, 
  pat=c("^commit\\.command$", "\\.Rproj$", "^__.*", "^\\.DS_Store$"),
  verbose=TRUE) {
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

gitcheck <- function() {
	g <- try(system2("git", "--version", stdout=TRUE), silent=TRUE)
	if (class(g) == "try-error") {
		Sys.sleep(0.2)
		stop("could not execute git command. ", 
		      "Make sure git is installed and on the PATH.\n  ",
		      "(See: Sys.getenv(\"PATH\"))" )
	}
}
gitcheck()

system2("git", "init")


