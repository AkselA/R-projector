
library(roxygen2)

setwd("/Users/aksel.henriksen/Documents/R/prosjekter/")

projname <- "projector"

sapply(list.files(file.path(projname, "R"), full.names=TRUE), source)

roxygenize(projname)

pkg_data(projname)
pkg_objects(projname)
pkg_check(projname)
r_manual()
pkg_install(projname)
library(projname, character.only=TRUE)

