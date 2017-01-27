#! /usr/bin/env Rscript
options(repos = structure(c(CRAN = "http://cran-mirror.cs.uu.nl/")))
packages <- c("R.utils", "R.methodsS3", "R.oo", "testthat", "roxygen2")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
update.packages(lib.loc = Sys.getenv("R_LIBS_USER"), ask = FALSE)
