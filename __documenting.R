
projname <- "projector"
library(roxygen2)
library(remotes)
setwd("/Users/aksel.henriksen/Documents/R/prosjekter/")
sapply(list.files(file.path(projname, "R"), full.names=TRUE), source)

git_init(projname)

roxygenize(projname)
pkg_pdf(projname)

pkg_data(projname)
pkg_objects(projname)
pkg_check(projname)
r_manual()
pkg_install(projname)
library(projname, character.only=TRUE)

git_commit(projname, "Changed functions so they operate from pwd by default", push=TRUE)
remotes::install_github("AkselA/R-projector")



