
data_obj <- function(projname=".", lib.loc=".") {
    if (projname == ".") {
		odir <- getwd()
		projname <- basename(odir)
		on.exit(setwd(odir))
		setwd(dirname(odir))
	}
    pkgpath <- find.package(projname, lib.loc, quiet=TRUE)
    if (!length(pkgpath)) {
        stop("there is no package called '", projname, "'", domain=NA)
    }
    dataDir <- file.path(pkgpath, "data")
    files <- tools::list_files_with_type(dataDir, "data")
    files0 <- basename(files)
    files <- basename(tools::file_path_sans_ext(files, TRUE))
    dupes <- duplicated(files)
    if (sum(dupes) > 0) {
        warning("Duplicate names: ", paste(files0[dupes], collapse=", "))
    }  
    files <- files[!dupes]
    files0 <- files0[!dupes]
    ans <- vector("list", length(files))
    dataEnv <- new.env(hash=TRUE)
    names(ans) <- files
    siz <- ans
    cls <- ans
    for (f in files) {
        utils::data(list=f, package=projname, lib.loc=lib.loc, envir=dataEnv)
        nam <- ls(envir=dataEnv, all.names=TRUE)
        mgt <- mget(nam, envir=dataEnv, inherits=TRUE)
        ans[[f]] <- nam
        siz[[f]] <- sapply(mgt, object.size)
        cls[[f]] <- sapply(mgt, class)
        rm(list=ans[[f]], envir=dataEnv)
    }
    names(ans) <- files0
    ans <- cbind(stack(ans), stack(siz)[,1], stack(cls)[,1],
      stringsAsFactors=FALSE)
    ans$ind <- as.character(ans$ind)
    colnames(ans) <- c("obj.name", "file.name", "size", "class")
    ans
}


code_obj <- function(projname=".", lib.loc=".") {
    if (projname == ".") {
		odir <- getwd()
		projname <- basename(odir)
		on.exit(setwd(odir))
		setwd(dirname(odir))
	}
    pkgpath <- find.package(projname, lib.loc, quiet=TRUE)
    if (!length(pkgpath)) {
        stop("there is no package called '", projname, "'", domain=NA)
    }
    dataDir <- file.path(pkgpath, "R")
    files <- tools::list_files_with_type(dataDir, "code")
    files0 <- basename(files)
    files1 <- basename(tools::file_path_sans_ext(files, TRUE))
    dupes <- duplicated(files1)
    if (sum(dupes) > 0) {
        warning("Duplicate names: ", paste(files0[dupes], collapse=", "))
    }  
    files <- files[!dupes]
    files0 <- files0[!dupes]
    ans <- vector("list", length(files))
    codeEnv <- new.env(hash=TRUE)
    names(ans) <- files0
    siz <- ans
    cls <- ans
    for (f in seq_along(files)) {
        source(file=files[f], local=codeEnv)
        nam <- ls(envir=codeEnv, all.names=TRUE)
        mgt <- mget(nam, envir=codeEnv, inherits=TRUE)
        ans[[f]] <- nam
        siz[[f]] <- sapply(mgt, object.size)
        cls[[f]] <- sapply(mgt, class)
        rm(list=ans[[f]], envir=codeEnv)
    }
    names(ans) <- files0
    ans <- cbind(stack(ans, stringsAsFactors=FALSE), stack(siz)[,1], stack(cls)[,1],
      stringsAsFactors=FALSE)
    ans$ind <- as.character(ans$ind)
    colnames(ans) <- c("obj.name", "file.name", "size", "class")
    ans
}

as.object_size <- function(x) {
	class(x) <- "object_size"
	x
}

size_units <- function(x, units="auto", standard="SI", ...){
	class(x) <- "object_size"
	format(x, units=units, standard=standard, ...)
}

envir.exists <- function(env) {
	if (is.character(env)) {
		env <- get(env)
	}
    tryCatch(is.environment(env), error=function(e) FALSE)
}

person_call <- function(x) {
	pp <- unclass(x)[[1]]
	ll <- lengths(pp) > 1
	pp <- lapply(pp, shQuote, type="cmd")
	pp[ll] <- lapply(pp[ll], function(x) paste0("c(", paste(x, collapse=", "), ")"))

    paste0("person(given=", pp$given, ", family=", pp$family,  
      ", email=", pp$email, ", role=", pp$role, ", comment=", pp$comment, ")")
}

# g <- getOption("devtools.desc.author")
# p <- person_call(as.person(g))
# cat(p)
# eval(parse(text=p))
package?devtools