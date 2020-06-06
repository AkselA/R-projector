
projname <- "projector"

setwd("/Users/aksel.henriksen/Documents/R/prosjekter/projector")
sapply(list.files("R", full.names=TRUE), source)

pkg_data()
roxygen2::roxygenize()
pkgload::dev_help("pkg_data")
increment_version()
pkg_pdf()

pkg_objects()
pkg_check()
r_manual()
pkg_install()
?projector::increment_version

library(projname, character.only=TRUE)
pkg_detach()


git_commit(msg="removed .DS_Store", push=TRUE)
remotes::install_github("AkselA/R-projector")

packageDescription("roxygen2")
str(gg)
library(dplyr)

library(remotes)

p <- as.person(gg)

p