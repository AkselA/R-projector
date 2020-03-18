
projname <- "projector"
library(roxygen2)
library(remotes)
setwd("/Users/aksel.henriksen/Documents/R/prosjekter/projector")
sapply(list.files("R", full.names=TRUE), source)

git_init()

roxygen2::roxygenize()
pkg_pdf()

pkg_data()
pkg_objects()
pkg_check()
r_manual()
pkg_install()

library(projname, character.only=TRUE)

git_commit(msg="Testing functions from pwd", push=TRUE)
remotes::install_github("AkselA/R-projector")



