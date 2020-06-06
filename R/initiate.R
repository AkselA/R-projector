initiate <- function(projname, verbose=TRUE, git=FALSE, remote) {
    old.dir <- getwd()
    dir.create(projname)
    setwd(projname)
    on.exit(setwd(old.dir))
    a <- 12
    b <- TRUE
    l <- lapply(c("data", "man", "R", "exec", "inst"), dir.create)
    
    # imports <- installed.packages(, "high")[, c(1, 4)]
    # imports <- imports[imports[,"Priority"] == "base", "Package"]
    # imports <- imports[imports != "base"]
    
    # imports <- getOption("defaultPackages")
    
    imports <- c("utils", "tools")
    
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
    if (git | !missing(remote)) {
        g1 <- "\ngit_commit(msg='Zapped all critical bugs')\n" 
    }

    if (!missing(remote)) {
        g2 <- paste(
          "git_push()", 
          sprintf("remotes::install_github('%s')", remote),
          sep="\n")
    }
    
    doctext <- paste(
      sprintf("setwd('%s')", getwd()),
      sprintf("projname <- '%s'", projname),
      "# sapply(list.files('R', full.names=TRUE), source)",
      "",
      "roxygen::roxygenize(projname)",
      "pkg_pdf()",
      "",
      "pkg_data()",
      "pkg_objects()",
      "pkg_check()",
      "r_manual()",
      "pkg_install()",
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
    dtext <- paste(
      "# This file will not be included in the build of the package.",
      "# By using add_data(), objects created here will be available",
      "# from the 'data/' folder\n",
      "exdat <- data.frame(Akka=c(1, 1, 2, 3, 3), Bakka=letters[c(4, 5, 5, 6, 6)],",
      "  stringsAsFactors=FALSE)\n",
      sep="\n")

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
    auth <- getOption("devtools.desc.author")

    if (length(auth)) {
    	auth <- person_call(as.person(auth))
    } else {
    	auth <- paste("person(\"Given Name\", \"Family Name\",", 
          "email=\"abc@mail.com\", role=c(\"aut\", \"cre\"))")
    }

    lic <- getOption("devtools.desc.license")

    if (!length(lic)) {
    	lic <- "GPL-3"
    }

    descv <- c(
      Package = projname,
      Version = "0.1.0.9000",
      Date = as.character(Sys.Date()), 
      Title = "What Projtest is (One Line, Title Case Required)", 
      `Authors@R` = auth,
      Description = paste("A (one paragraph) description of what", projname,
        "does and why it may be useful."), 
      Depends = paste0("R (>= ", getRversion(), ")"),
      Imports = paste(imports, collapse=", "),
      Suggests = "MASS", 
      License = lic,
      URL = "http.example.com",
      LazyData = "true",
      Encoding = "UTF-8"
    )

    desc <- strwrap(paste0(names(descv), ": ", descv), 80, exdent=4)
    
    if (verbose) {
        message("\nDESCRIPTION:")
        cat(desc, sep="\n")
    }
    cat(desc, sep="\n", file="DESCRIPTION")
    
    ### NAMESPACE
    nams <- sprintf("import(%s)", imports)

    if (verbose) {
        message("\nNAMESPACE:")
        cat(nams, sep="\n")
    }
    cat(nams, sep="\n", file="NAMESPACE")
    
    ### R/0_<projname>-package.R
    doc_pack <- paste0("#' A short description of the package\n",
                       "#' \n",
                       "#' A more detailed description of the package\n",
                       "#' \n",
                       "#' @section Details:\n",
                       "#' Highligh central functions, quick-start guide, etc.\n",
                       "#' \n",
                       "#' @section ", projname, " contributors:\n",
                       "#' \n",
                       "#' @docType package\n",
                       "#' @name ", projname, "-package\n",
                       "#' @rdname ", projname, "\n",
                       "\n",
                       "NULL\n\n")
    
    if (verbose) {
        message("\nR/0_", projname, "-package.R:")
        cat(doc_pack)
    }
    cat(doc_pack, file=sprintf("R/0_%s-package.R", projname))
    
    ### R/0_data
    doc_data <- c(
      "# An example of how to document a dataset",
      "",
      "#' Title of the dataset",
      "#' ",
      "#' @details",
      "#' A more detailed description of the dataset",
      "#' ",
      "#' @section Further notes",
      "#' Use an itemized list to go into more detail",
      "#' \\describe{",
      "#'   \\item{Step 1}{run add_data() to move datasets from __data.R to data/}",
      "#'   \\item{Step 2}{run roxygenize() to build dataset documentation}",
      "#' }",
      "#' ",
      "#' You can also (as of Roxygen2 7.0.0) create tables, if that helps in",
      "#' describing the data:",
      "#' | Akka | Bakka |",
      "#' | :-:  | :-    |",
      "#' | num  | char  |",
      "#' ",
      "#' @format",
      "#' A 3x2 data.frame",
      "#' ",
      "#' @source",
      "#' Where the dataset comes from. Maybe a scientific article, book or website.",
      "#' Adding a url, if applicable, is nice.",
      "#' <https://github.com/AkselA/R-projector/blob/master/__data.R>",
      "#' ",
      "#' @examples",
      "#' # add some examples of what you can do with the dataset",
      "#' ",
      "#' table(exdat)",
      "#' with(exdat, tapply(Akka, Bakka, mean))",
      "",
      "\"exdat\"",
      "")

    if (verbose) {
        message("\nR/0_data:")
        cat(doc_data, sep="\n")
    }
    cat(doc_data, sep="\n", file="R/0_data.R")

    if (git) {
        git_init() 
    }

    if (!missing(remote)) {
        git_remote(repo=remote)
        git_push()
    }

}
