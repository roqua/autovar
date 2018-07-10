#! /usr/bin/env Rscript

options(repos=structure(c(CRAN="http://cran.uni-muenster.de/")))
packages <- c("Amelia", "e1071", "foreign", "ggplot2", "gridExtra", "igraph", "jsonlite", "knitr", "markdown", "norm", "parallel", "psych", "RcppArmadillo", "reshape2", "stringi", "stringr", "TimeProjection", "urca", "vars", "roxygen2", "testthat");
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])];
if(length(new.packages))
  install.packages(new.packages);
update.packages(lib.loc=Sys.getenv("R_LIBS_USER"), ask=FALSE);
