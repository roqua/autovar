#! /usr/bin/env Rscript

options(repos=structure(c(CRAN="http://cran-mirror.cs.uu.nl/")))
packages <- c("Amelia", "e1071", "foreign", "ggplot2", "gridExtra", "igraph", "jsonlite", "knitr", "markdown", "norm", "parallel", "psych", "RcppArmadillo", "reshape2", "stringi", "stringr", "TimeProjection", "urca", "vars", "roxygen2", "testthat", "devtools");
gh_packages <- list(
  list(repo = "compsy/vars", branch = 'master')
)

install <- function(packages, installfunc, ...){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages))
    installfunc(new.packages, ...)
  update.packages(lib.loc = Sys.getenv("R_LIBS_USER"), ask = FALSE)
}

install(packages, install.packages)
lapply(gh_packages, function(pkg) install(pkg$repo, devtools::install_github, ref = pkg$branch))


