# turns objects found in "projname"/data.R (project root)
# into data files available through data()
# by saving them as .rda files in "projname"/data
pkg_data <- function(projname=".", add=TRUE) {
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
        unlink(datadir, recursive=TRUE)
    }
    tmp.env <- new.env()
    source(datapath, local=tmp.env)
    tmp.list <- as.list(tmp.env, sorted=TRUE)
    files <- file.path(projname, "data", paste0(names(tmp.list), ".rda"))
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

# Create and show documentation PDF
pkg_pdf <- function(projname=".", popt="--force") {
    if (projname != ".") {
	    if (!dir.exists(projname)) {
	        stop(paste("Could not find", projname, "in current directory"))
	    }
        owd <- getwd()
    	on.exit(setwd(owd))
        setwd(projname)
    }

    R <- file.path(R.home("bin"), "R")

    popts <- paste(popt, collapse=" ")
    parg <- paste("CMD", "Rd2pdf", popts, shQuote(getwd()))
    system2(R, parg)
}

#' Bild and run diagnostic checks on the package
#' 
#' @param projname path to the project file
#' @param bopt \code{R CMD build} options
#' @param copt \code{R CMD check} options
#' @param ropt \code{R} options
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
	  function(x) format(as.object_size(x), units="auto", standard="SI"))
    obj[c("size.SI", "obj.name", "file.name", "folder", "class", "size")]
}