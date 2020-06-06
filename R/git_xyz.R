
git_check <- function() {
    g <- try(system2("git", "--version", stderr=FALSE, stdout=TRUE), silent=TRUE)
    if (class(g) == "try-error") {
        stop("Could not execute git command. ", 
              "Make sure git is installed and on the PATH.\n  ",
              "(See: Sys.getenv(\"PATH\"))" )
    }
}

github_check <- function(repo) {
    g1 <- system2("ssh", "-T git@github.com", stderr=FALSE)
    if (g1 != 1) {
        stop("Could not find public SSH key. ",
        "Consult https://help.github.com/en/github/authenticating-to-github",
        "/error-permission-denied-publickey for troubleshooting.")
    }
    rurl <- paste0("git@github.com:", repo)
    arg <- paste("ls-remote", rurl)
    g2 <- system2("git", arg)
    if (g2 != 0) {
        Sys.sleep(0.2)
        cat("\n")
        stop("Could not find ", rurl)
    }
}

#' Initialize git
#' 
#' Create an empty Git repository or reinitialize an existing one
#' 
#' @param projname path to a package source, by default "." (current directory)
#' @param add what files to include, by default "." (all)
#' @param ignore patterns to be written to .gitignore
#' @param commit logical; should changes be commited?
#' @param msg the message to be included with the commit
#' 
#' @details
#' An empty Git repository is created, files added, and a .gitignore file generated.
#' Optionally the changes are commited and stored with a message.
#' 
#' @family git functions
#' 
#' @export

git_init <- function(projname=".", add=".", ignore=c(".DS_Store", ".RData",
  ".Rhistory", ".Rapp.history", ".Rproj.user"), commit=TRUE, msg="first commit") {
    git_check()
    if (projname != ".") {
        od <- getwd()
        on.exit(setwd(od))
        setwd(projname)
    }
    message(".gitignore:")
    cat(ignore, "", sep="\n")
    cat(ignore, "", sep="\n", file=".gitignore")
    system2("git", "init")
    system2("git", paste("add", paste(shQuote(add), collapse=" ")))
    if (commit) {
        system2("git", paste("commit -m", shQuote(msg)))
    }
}

#' Add git remote
#' 
#' Add a remote at a given github repository
#' 
#' @param projname path to a folder with a git repository, by default "."
#' (current directory)
#' @param repo repository address in the format "username/repo[/subdir]"
#' 
#' @details
#' Connects a GitHub remote to the given git repository. This assumes a GitHub
#' repository and SSH is already set up.
#' 
#' @family git functions
#' 
#' @export

git_remote <- function(projname=".", repo) {
    git_check()
    github_check(repo)
    if (projname != ".") {
        od <- getwd()
        on.exit(setwd(od))
        setwd(projname)
    }
    system2("git", paste0("remote add origin git@github.com:", repo))
}

#' Git commit
#' 
#' Commit (and push) changes made to the git repository
#' 
#' @param projname path to a folder with a git repository, by default "."
#' (current directory)
#' @param msg message to be included with the commit
#' @param add what files to include, either as a single character string, 
#' or a character vector, where each element is a directory, file or fileglob.
#' @param push logical; should the changes be pushed to a remote repository?
#' 
#' @details
#' This will add and commit, plus optionally push, all changes made in the given
#' repository. Push assumes a very basic setup and pushes upstream to the master
#' branch of 'origin'.
#' 
#' @family git functions
#' 
#' @export

git_commit <- function(projname=".", msg, add=".", push=FALSE) {
    git_check()
    if (missing(msg)) {
        stop("Please supply a short commit message")
    }
    if (projname != ".") {
        od <- getwd()
        on.exit(setwd(od))
        setwd(projname)
    }
    system2("git", paste("add", paste(shQuote(add), collapse=" ")))
    system2("git", paste("commit -m", shQuote(msg)))
    if (push) {
        system2("git", "push -u origin master")
    }
}

#' Git push
#' 
#' Push changes to a remote repository
#' 
#' @param projname path to a folder with a git repository, and a remote cennected.
#' By default "." (current directory)
#' 
#' @details
#' This will push all changes made in the given local repository to the
#' master branch of the 'origin' remote repository.
#' 
#' @family git functions
#' 
#' @export

git_push <- function(projname=".") {
    if (projname != ".") {
        od <- getwd()
        on.exit(setwd(od))
        setwd(projname)
    }
    system2("git", "push -u origin master")
}
