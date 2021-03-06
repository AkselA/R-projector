% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/git_xyz.R
\name{git_push}
\alias{git_push}
\title{Git push}
\usage{
git_push(projname = ".")
}
\arguments{
\item{projname}{path to a folder with a git repository, and a remote cennected.
By default "." (current directory)}
}
\description{
Push changes to a remote repository
}
\details{
This will push all changes made in the given local repository to the
master branch of the 'origin' remote repository.
}
\seealso{
Other git functions: \code{\link{git_commit}},
  \code{\link{git_init}}, \code{\link{git_remote}}
}
\concept{git functions}
