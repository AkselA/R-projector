initiate <- function(projname, verbose=TRUE, git=TRUE, remote) {
    old.dir <- getwd()
    dir.create(projname)
    setwd(projname)
    
    l <- lapply(c("data", "man", "R", "exec", "inst"), dir.create)
    
    # imports <- installed.packages(, "high")[, c(1, 4)]
    # imports <- imports[imports[,"Priority"] == "base", "Package"]
    # imports <- imports[imports != "base"]
    imports <- "utils"
    
    
    
    ### README
    readmetext <- paste(
      paste("#", projname), "An R project under development\n",
      "### Usage",
      "```R",
      "# put some example code here",
      "```  \n", sep="\n"
    )
    if (verbose) {
        message("README.md:")
        cat(readmetext)
    }
    cat(readmetext, file="README.md")
    
    ### __documenting
    g1 <- ""
    g2 <- ""
    if (git) {
    	g1 <- "\ngit_commit(projname, 'Zapped all critical bugs')\n" 
    }

    if (!missing(remote)) {
    	g2 <- "git_push(projname)\n"
    }
    
    doctext <- paste(
      sprintf("setwd('%s')", old.dir),
      sprintf("projname <- '%s'", projname),
      "# sapply(list.files(file.path(projname, \"R\"), full.names=TRUE), source)",
      "",
      "roxygenize(projname)",
      "",
      "pkg_data(projname)",
      "pkg_objects(projname)",
      "pkg_check(projname)",
      "r_manual()",
      "pkg_install(projname)",
      "library(projname, character.only=TRUE)",
      g1,
      g2,
      sep="\n"
    )
    if (verbose) {
        message("\n__documenting.R:")
        cat(doctext)
    }
    cat(doctext, file="__documenting.R")

    ### __data.R
    dtext <- paste("# This file will not be included in the build of the package.",
                   "# By using add_data(), objects created here will be available",
                   "# from the 'data/' folder\n", sep="\n")

    if (verbose) {
        message("\n__data.R:")
        cat(dtext)
    }
    cat(dtext, file="__data.R")
        
    ### .Rbuildignore
    pat <- c("^commit\\.command$", "\\.Rproj$", "^__.*", "^\\.DS_Store$")
    pat <- paste(pat, collapse="\n")
    
    if (verbose) {
        message("\n.Rbuildignore:")
        cat(pat, "\n")
    }
    cat(pat, "\n", file=".Rbuildignore")
    
    ### R/0_imports.R
    imptext <- paste0(
               paste(
               paste0("#' @import ", imports), collapse="\n"), "\n\nNULL\n")
    
    if (verbose) {
        message("\nR/0_imports.R:")
        cat(imptext)
    }
    cat(imptext, file="R/0_imports.R")
    
    ### DESCRIPTION
    imptext2 <- paste(
                strwrap(
                paste(imports, collapse=", "), 50, exdent=9), collapse="\n")
    
    desc <- paste0(
        "Package: ", projname, "\n",
        "Version: 0.1\n",
        "Date: ", Sys.Date(), "\n",
        tools::toTitleCase(paste(
        "Title: what", projname, "is (one line, title case required)\n")),
        'Authors@R: person("First Name", "Last", email="abc@mail.com",\n',
        '                   role=c("aut", "cre"))\n',
        "Description: A (one paragraph) description of what ", projname, "\n",
        "             does and why it may be useful.\n",
        "Depends: R (>= ", getRversion(), ")\n",
        "Imports: ", imptext2, "\n",
        "Suggests: MASS\n",
        "License: GPL-3\n",
        "URL: https://www.homepage.org, http://www.another.url\n",
        "LazyData: true\n",
        "Encoding: UTF-8\n")
    
    if (verbose) {
        message("\nDESCRIPTION:")
        cat(desc)
    }
    cat(desc, file="DESCRIPTION")
    
    ### NAMESPACE
    nams <- paste0("import(", imports, ")\n")

    if (verbose) {
        message("\nNAMESPACE:")
        cat(nams)
    }
    cat(desc, file="NAMESPACE")
    
    ### R/0_doc_package.R
    doc_pack <- paste0("#' ", projname, ": A short description of the package.\n",
                       "#' \n",
                       "#' A more detailed explanation/description of the package\n",
                       "#' \n",
                       "#' @section ", projname, " functions:\n",
                       "#' \\code{\\link{function1}} Short description\n",
                       "#' \\code{\\link{function2}} Short description\n",
                       "#' \n",
                       "#' @docType package\n",
                       "#' @name ", projname, "\n",
                       "\n",
                       "NULL\n\n")
    
    if (verbose) {
        message("\nR/0_doc_package.R:")
        cat(doc_pack)
    }
    cat(doc_pack, file="R/0_doc_package.R")

    if (git) {
    	git_init(".") 
    }

    if (!missing(remote)) {
    	git_remote(".", remote)
    }

}
