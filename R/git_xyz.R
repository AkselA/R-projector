
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

git_init <- function(projname, msg="first commit", add=".") {
	git_check()
	od <- getwd()
	on.exit(setwd(od))
	setwd(projname)
    system2("git", "init")
	system2("git", paste("add", paste(shQuote(add), collapse=" ")))
    system2("git", paste("commit -m", shQuote(msg)))
}

git_commit <- function(projname, msg, add=".") {
	git_check()
	if (missing(msg)) {
		stop("Please supply a short commit message")
	}
	od <- getwd()
	on.exit(setwd(od))
	setwd(projname)
	system2("git", paste("add", paste(shQuote(add), collapse=" ")))
    system2("git", paste("commit -m", shQuote(msg)))
}

github_connect <- function(projname, repo) {
	git_check()
	github_check()
	od <- getwd()
	on.exit(setwd(od))
	setwd(projname)
    system2("git", paste0("remote add origin git@github.com:", repo))
    system2("git", "push -u origin master")
}


git_push <- function(projname) {
	od <- getwd()
	on.exit(setwd(od))
	setwd(projname)
    system2("git", "push -u origin master")
}