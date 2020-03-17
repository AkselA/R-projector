
projname <- "projector"
library(roxygen2)
setwd("/Users/aksel.henriksen/Documents/R/prosjekter/")
sapply(list.files(file.path(projname, "R"), full.names=TRUE), source)

git_init(projname)

roxygenize(projname)

pkg_data(projname)
pkg_objects(projname)
pkg_check(projname)
r_manual()
pkg_install(projname)
library(projname, character.only=TRUE)

git_commit(projname, "Updated initiate() to include git", push=TRUE)