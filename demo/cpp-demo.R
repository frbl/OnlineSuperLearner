
## Following the guide of the SL3 package
## Requires:
## - tidyverse
## - sl3
## - R.utils
## - OnlineSuperLearner
## - devtools

## Load the OSL package
devtools::load_all(".")

## Dont ask for input while plotting
devAskNewPage(ask = FALSE)

######################
## Helper functions ##
######################
## Function to create a formula notation
generate_fomula <- function(dependent, independent) {
  form <- lapply(dependent, function(dep) {
    s <-dep 
    first <- TRUE
    for (dep_in in independent) {
      if(dep_in == dep) next
      if(first) s <- paste(s, dep_in, sep = ' ~ ')
      else s <- paste(s, dep_in, sep = ' + ')
      first <- FALSE
    }
    first <- TRUE
    formula(s)
  })
  names(form) <- dependent
  form
}

## Function to create a formula notation for a set of variables
generate_formulae <- function(W, A, Y){
  ## Generate W Formulae
  W_form <- generate_fomula(W, W) 
  A_form <- generate_fomula(W, W) 
  Y_form <- generate_fomula(Y, c(W,A)) 
  list(W=W_form, A=A_form, Y=Y_form)
}
######################


set.seed(49753)
library(sl3)
library(tidyverse)

## Load the cpp data
data(cpp)
cpp <- cpp %>%
  dplyr::filter(!is.na(haz)) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

## Define the covariates
W <-c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
A <- c()
Y <- c("haz")

## Generate the corresponding formulae and randomvariables
formulae <- generate_formulae(W,A,Y)
randomVariables <- list()
## randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$apgar1, family = 'gaussian'))
## randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$apgar5, family = 'gaussian'))
## randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$parity, family = 'gaussian'))
## randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$gagebrth, family = 'gaussian'))
## randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$meducyrs, family = 'gaussian'))
## randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$sexn, family = 'gaussian'))
randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$Y$haz, family = 'gaussian'))

## Define a list of algorithms to use
algos <- list()

algos <- append(algos, list(list(algorithm = "ML.XGBoost",
                                 algorithm_params = list(alpha = c(0.3, 0.4, 0.5)),
                                 params = list(nbins = c(30, 40, 50), online = TRUE))))

algos <- append(algos, list(list(algorithm = "condensier::speedglmR6",
                                 params = list(nbins = c(5, 10, 15), online = FALSE))))

## General settings
log <- R.utils::Arguments$getVerbose(-1, timestamp=TRUE)
nb_iter <- 10
training_set_size <- nrow(cpp)

## Fit the online SuperLearner
osl <- OnlineSuperLearner::fit.OnlineSuperLearner(
  formulae = randomVariables,
  data = cpp,
  algorithms = algos, 
  verbose = log,

  initial_data_size = training_set_size / 2,
  max_iterations = nb_iter,
  mini_batch_size = (training_set_size / 2) / nb_iter
)

## With the fitted OSL we can now sample and perform predictions
preds <- sampledata(osl, newdata = cpp, randomVariables, plot = TRUE)
preds$osl.estimator %>% print
preds$dosl.estimator %>% print

## With the fitted OSL we can now sample and perform predictions
preds <- predict(osl, newdata = cpp, randomVariables, plot = TRUE)
preds$osl.estimator %>% print
preds$dosl.estimator %>% print



