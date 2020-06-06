# turns objects found in "projname"/data.R (project root)
# into data files available through data()
# by saving them as .rda files in "projname"/data

#' Prepare datasets
#' 
#' Save R objects as datasets
#' 
#' @param projname path to a package source, by default "." (current directory)
#' @param add logical; should datasets be added to those already existing in \code{data/}?
#' 
#' @details
#' All R objects in \code{__data.R} are saved to the folder \code{data/} as files on the
#' pattern \code{d_<obj>.rda}, one for each object. If \code{add=FALSE}, then any existing
#' files of that pattern will first be removed, before new ones are saved.
#' 
#' @return
#' \code{.rda} file(s) saved in the \code{data/} folder.
#' 
#' @seealso \code{\link{pkg_objects}}
#'  
#' @export

pkg_data <- function(projname=".", add=FALSE) {
    if (projname == ".") {
		odir <- getwd()
		projname <- basename(odir)
		on.exit(setwd(odir))
		setwd(dirname(odir))
	}
    if (!dir.exists(projname)) {
        stop(paste("Could not find", projname, "in current directory"))
    }
    datapath <- file.path(projname, "__data.R")
    if (!file.exists(datapath)) {
        stop(paste("Could not find", datapath))
    }
    datadir <- file.path(projname, "data")
    if (!dir.exists(datadir)) {
    	dir.create(datadir)
    }
    if (!add) {
        unlink(list.files(datadir, "^d_.*\\.rda$"))
    }
    tmp.env <- new.env()
    source(datapath, local=tmp.env)
    tmp.list <- as.list(tmp.env, sorted=TRUE)
    files <- file.path(projname, "data", paste0("d_", names(tmp.list), ".rda"))
    obj <- mapply(save, list=names(tmp.list), file=files, 
      MoreArgs=list(compress="xz", envir=tmp.env))
    if (length(files) == 1) {
        cat("File added:")
    } else {
        cat("Files added:")
    }
    dtf <- data.frame(x=paste0(files), 
                      y=paste(sprintf("%.1f", file.size(files)/1000), "kB"))
    names(dtf) <- c(" ", " ")
    dtf
}

#' Object sizes
#' 
#' Find the size of a package's loadable objects
#' 
#' @param projname path to a package source, by default "." (current directory)
#' 
#' @details
#' The source code of a package is searched for function and dataset objects, found
#' in the \code{R/} and \code{dataset/} directories.
#' 
#' @return
#' A data.frame giving the size, name, class, source file name and folder for each object
#' 
#' @seealso \code{\link{pkg_data}}
#' 
#' @export

pkg_objects <- function(projname=".") {
    if (projname == ".") {
		odir <- getwd()
		projname <- basename(odir)
		on.exit(setwd(odir))
		setwd(dirname(odir))
	}
	da <- cbind(data_obj(projname, lib.loc="."), folder="data",
	  stringsAsFactors=FALSE)
	co <- cbind(code_obj(projname, lib.loc="."), folder="R",
	  stringsAsFactors=FALSE)
	obj <- rbind(da, co)
	obj <- obj[order(obj$size, decreasing=TRUE),]
	rownames(obj) <- NULL
	obj$size.SI <- sapply(obj$size, 
	  function(x) size_units(x, digits=0))
    obj <- obj[c("size.SI", "obj.name", "class", "file.name", "folder", "size")]
    print(obj)
	cat("\nTotal size:", size_units(sum(obj$size)), "\n")
	invisible(obj)
}

#' PDF manual
#' 
#' Render the PDF manual of all the package's documentation
#' 
#' @param projname path to a package source, by default "." (current directory)
#' @param popt a character vector, or space-delimited character string, of options
#' to be passed to \code{\link{Rd2pdf}}. By default \code{--force} to overwrite existing
#' file
#' @param ropt a character vector, or space-delimited character string, of options
#' to be passed to \code{R}. By default \code{--vanilla} to run in a baseline R
#' session, reading no profile or environment files, and no saving of workspace
#' 
#' @return
#' A PDF file is saved to the root directory of the package
#' 
#' @seealso \code{\link{Rd2pdf}}
#' 
#' @export

pkg_pdf <- function(projname=".", ropt="--vanilla", popt="--force") {
    if (projname != ".") {
	    if (!dir.exists(projname)) {
	        stop(paste("Could not find", projname, "in current directory"))
	    }
        owd <- getwd()
    	on.exit(setwd(owd))
        setwd(projname)
    }

    R <- file.path(R.home("bin"), "R")

    ropts <- paste(ropt, collapse=" ")

    popts <- paste(popt, collapse=" ")
    parg <- paste(ropts, "CMD", "Rd2pdf", popts, shQuote(getwd()))
    system2(R, parg)
}

#' Package check
#' 
#' Build and run diagnostic checks on the package
#' 
#' @param projname path to the project file
#' @param bopt a character vector, or space-delimited character string, of options
#' to be passed to \code{\link{build}}. By default \code{--no-manual}
#' @param copt a character vector, or space-delimited character string, of options
#' to be passed to \code{\link{check}}. By default \code{--vanilla} to run in a baseline R
#' session, reading no profile or environment files, and no saving of workspace
#' @param ropt a character vector, or space-delimited character string, of options
#' to be passed to \code{R}. By default \code{--vanilla} to run in a baseline R
#' session, reading no profile or environment files, and no saving of workspace
#' @param rm.src remove source file
#' 
#' @export 

pkg_check <- function(projname=".", bopt=c("--no-manual"), 
  copt=c("--no-manual", "--timings"), ropt=c("--vanilla"), rm.src=TRUE) {
    if (projname == ".") {
		odir <- getwd()
		projname <- basename(odir)
		on.exit(setwd(odir))
		setwd(dirname(odir))
	}
    if (!dir.exists(projname)) {
        stop(paste("Could not find", projname, "from current directory"))
    }
    out <- grep("--output=", copt)
    if (length(out) > 0) {
        outdir <- sub("--output=", "", copt[out])
    } else {
        outdir <- tempdir()
        copt <- c(copt, paste0("--output=", outdir))
    }
    
    pkg <- file.path(getwd(), projname)
    dcr <- desc_read(projname, quiet=TRUE)
    tgz <- paste0(dcr["Package"], "_", dcr["Version"], ".tar.gz")
    R <- file.path(R.home("bin"), "R")
    ropts <- paste(ropt, collapse=" ")
    
    bopts <- paste(bopt, collapse=" ")
    barg <- paste(ropts, "CMD", "build", bopts, shQuote(pkg))
    system2(R, barg)

    copts <- paste(copt, collapse=" ")
    carg <- paste(ropts, "CMD", "check", copts, tgz)
    system2(R, carg)
    
    if (rm.src) {
        # cwd <- system2(R, "--no-save --slave -e 'cat(getwd())'", stdout=TRUE)
        file.remove(tgz)
    }
    logpath <- file.path(outdir, paste0(projname, ".Rcheck"), "00check.log")
    chlog <- readLines(logpath)
    Sys.sleep(1)
    cat("*** LOG ***", chlog, sep="\n")
    invisible(chlog)
}

#' Install a package
#' 
#' @param projname path to the project file
#' @param bopt \code{R CMD build} options
#' @param iopt \code{R CMD install} options
#' @param ropt \code{R} options
#' @param rm.src remove source file
#' 
#' @export

pkg_install <- function(projname=".", bopt="", iopt="", ropt=c("--vanilla"),
  rm.src=TRUE) {
    if (projname == ".") {
		odir <- getwd()
		projname <- basename(odir)
		on.exit(setwd(odir))
		setwd(dirname(odir))
	}
    if (!dir.exists(projname)) {
        stop(paste("Could not find", projname, "from current directory"))
    }

    pkg <- file.path(getwd(), projname)
    dcr <- desc_read(projname, quiet=TRUE)
    tgz <- paste0(dcr["Package"], "_", dcr["Version"], ".tar.gz")
    R <- file.path(R.home("bin"), "R")
    
    bopts <- paste(bopt, collapse=" ")
    barg <- paste(ropt, "CMD", "build", bopts, shQuote(pkg))
    system2(R, barg)

    iopts <- paste(iopt, collapse=" ")
    iarg <- paste(ropt, "CMD", "INSTALL", iopts, tgz)
    system2(R, iarg)

    if (rm.src) {
        # cwd <- system2(R, "--no-save --slave -e 'cat(getwd())'", stdout=TRUE)
        file.remove(tgz)
    }
}

pkg_detach <- function(projname=basename(getwd())) {
	detach(paste0("package:", projname), character.only=TRUE)
}

# pkg_objects <- function(projname=".") {
    # if (projname == ".") {
		# odir <- getwd()
		# projname <- basename(odir)
		# on.exit(setwd(odir))
		# setwd(dirname(odir))
	# }
	# da <- cbind(data_obj(projname, lib.loc="."), folder="data",
	  # stringsAsFactors=FALSE)
	# co <- cbind(code_obj(projname, lib.loc="."), folder="R",
	  # stringsAsFactors=FALSE)
	# obj <- rbind(da, co)
	# obj <- obj[order(obj$size, decreasing=TRUE),]
	# rownames(obj) <- NULL
	# obj$size.SI <- sapply(obj$size, 
	  # function(x) format(as.object_size(x), units="auto", standard="SI"))
    # obj[c("size.SI", "obj.name", "file.name", "folder", "class", "size")]
# }