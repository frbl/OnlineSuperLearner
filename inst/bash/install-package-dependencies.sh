#! /usr/bin/env Rscript
# General packages
options(repos = structure(c(CRAN = "http://cran-mirror.cs.uu.nl/")))
packages <- c(
"devtools",
"RCurl",
"jsonlite",
"data.table",
"R.utils",
"R.methodsS3",
"R.oo",
"testthat",
"xgboost",
"roxygen2"
)
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
update.packages(lib.loc = Sys.getenv("R_LIBS_USER"), ask = FALSE)

# Specific packages
# Finds and remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
#install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/rel-kahan/5/R", getOption("repos"))))
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-tverberg/2/R")))

#devtools::install_github('cran/rkafka')
