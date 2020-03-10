# turns objects found in "projname"/data.R (project root)
# into data files available through data()
# by saving them as .rda files in "projname"/data
pkg_data <- function(projname, add=TRUE) {
    if (!dir.exists(projname)) {
        stop(paste("Could not find", projname, "in current directory"))
    }
    datapath <- file.path(projname, "__data.R")
    if (!file.exists(datapath)) {
        stop(paste("Could not find", datapath))
    }
    datadir <- file.path(projname, "data")
    if (!add) {
        unlink(datadir, recursive=TRUE)
    }
    dir.create(file.path(projname, "data"), showWarnings=FALSE)
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
pkg_pdf <- function(projname, popt="--force") {
    if (!dir.exists(projname)) {
        stop(paste("Could not find", projname, "in current directory"))
    }
    owd <- getwd()
    popts <- paste(popt, collapse=" ")

    R <- file.path(R.home("bin"), "R")
    pkg <- file.path(owd, projname) 
    parg <- paste("CMD", "Rd2pdf", popts, shQuote(pkg))

    setwd(projname)
    system2(R, parg)
    setwd(owd)
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

pkg_check <- function(projname, bopt=c("--no-manual"), 
  copt=c("--no-manual", "--timings"), ropt=c("--vanilla"), rm.src=TRUE) {
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
    bopts <- paste(bopt, collapse=" ")
    copts <- paste(copt, collapse=" ")
    
    pkg <- file.path(getwd(), projname)
    dcr <- descr(projname, quiet=TRUE)
    tgz <- paste0(dcr["Package"], "_", dcr["Version"], ".tar.gz")
    R <- file.path(R.home("bin"), "R")
    
    barg <- paste(ropt, "CMD", "build", bopts, shQuote(pkg))
    system2(R, barg)

    carg <- paste(ropt, "CMD", "check", copts, tgz)
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

pkg_install <- function(projname, bopt="", iopt="", ropt=c("--vanilla"),
  rm.src=TRUE) {
    if (!dir.exists(projname)) {
        stop(paste("Could not find", projname, "from current directory"))
    }

    bopts <- paste(bopt, collapse=" ")

    pkg <- file.path(getwd(), projname)
    dcr <- descr(projname, quiet=TRUE)
    tgz <- paste0(dcr["Package"], "_", dcr["Version"], ".tar.gz")
    R <- file.path(R.home("bin"), "R")
    
    barg <- paste(ropt, "CMD", "build", bopts, shQuote(pkg))
    system2(R, barg)

    iarg <- paste(ropt, "CMD", "INSTALL", iopt, tgz)
    system2(R, iarg)

    if (rm.src) {
        # cwd <- system2(R, "--no-save --slave -e 'cat(getwd())'", stdout=TRUE)
        file.remove(tgz)
    }
}

pkg_detach <- function(projname) {
	detach(paste0("package:", projname), character.only=TRUE)
}

pkg_objects <- function(projname) {
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