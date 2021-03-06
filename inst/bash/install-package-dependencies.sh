#!/usr/bin/env Rscript
message(R.version.string)
options(repos=structure(c(CRAN="https://cran.cnr.berkeley.edu/")))
install.packages('packrat')
packrat::restore()

#options(repos = structure(c(CRAN = "https://cran.cnr.berkeley.edu/")))

#packages <- c(
#"devtools",
#"ggplot2",
#"mockery",
#"RCurl",
#"jsonlite",
#"data.table",
#"R.utils",
#"R.methodsS3",
#"R.oo",
#"MASS",
#"Rcpp",
#"pROC",
#"testthat",
#"xgboost",
#"glmnet",
#"speedglm",
#"randomForest",
#"neuralnet",
#"e1071",
#"doRNG",
#"nnls",
#"dplyr",
#"magrittr",
#"assertthat",
#"optimr",
#"Metrics",
##"nloptr",
#"purrr",
#"doParallel",
#"foreach",
#"snow",
#"doSNOW",
#"matrixStats",
#"digest",
#"future",
#"roxygen2",
#"memoise",
#"simcausal",
#"withr",
#"httr"
#)

#gh_packages <- list(
  ##list(repo = "jeroenooms/jsonlite", branch = 'master'),
  ##list(repo = "osofr/simcausal", branch = 'master'),
  #list(repo = "osofr/condensier", branch = 'fb-add-update'),
  #list(repo = "jimhester/covr", branch = 'master'),
  #list(repo = "n-s-f/mockery", branch = 'master'),
  #list(repo = "r-lib/pkgdown", branch = 'master'),
  #list(repo = "airoldilab/sgd", branch = 'master')
  ##list(repo = "dmlc/xgboost/R-package", branch = 'master') #,
  ##list(repo = 'cran/rkafka', branch = 'master')
#)

#install <- function(packages, installfunc, ...){
  #new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  #if (length(new.packages))
    #installfunc(new.packages, ...)
  #update.packages(lib.loc = Sys.getenv("R_LIBS_USER"), ask = FALSE)
#}

#install(packages, install.packages)
#lapply(gh_packages, function(pkg) install(pkg$repo, devtools::install_github, ref = pkg$branch))


##if(unname((Sys.info()['nodename'] == 'frbl-mbp-2'))) {
  ##devtools::load_all('../../osofr/condensier')
##}


### Specific packages
### Finds and remove any previously installed H2O packages for R.
##if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
##if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

### Next, we download, install and initialize the H2O package for R.
###install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/rel-kahan/5/R", getOption("repos"))))
##install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-tverberg/2/R")))

