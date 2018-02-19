devtools::load_all(".")

## Following the guide of the SL3 package
set.seed(49753)
library(sl3)
library(tidyverse)
data(cpp)

cpp <- cpp %>%
  dplyr::filter(!is.na(haz)) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

covars <- 

W <-c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
A <- c()
Y <- c("haz")
formulae <- generate_formulae(W,A,Y)

randomVariables <- list()
#randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$apgar1, family = 'gaussian'))
#randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$apgar5, family = 'gaussian'))
#randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$parity, family = 'gaussian'))
#randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$gagebrth, family = 'gaussian'))
#randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$meducyrs, family = 'gaussian'))
#randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$W$sexn, family = 'gaussian'))
randomVariables <- append(randomVariables, RandomVariable$new(formula = formulae$Y$haz, family = 'gaussian'))

# Algos
algos <- list()

algos <- append(algos, list(list(algorithm = "ML.XGBoost",
                                 algorithm_params = list(alpha = c(0.3, 0.4, 0.5)),
                                 params = list(nbins = c(30, 40, 50, 60, 70, 80, 90), online = TRUE))))

algos <- append(algos, list(list(algorithm = "condensier::speedglmR6",
                                 ## algorithm_params = list(),
                                 params = list(nbins = c(5, 10, 15, 20, 25), online = FALSE))))

# bounds <- OnlineSuperLearner::PreProcessor.generate_bounds(data.train.static)
# pre_processor <- PreProcessor$new(bounds = bounds)

log <- FALSE
nb_iter <- 10
training_set_size <- nrow(cpp)

osl  <- OnlineSuperLearner::fit.OnlineSuperLearner(
  formulae = randomVariables,
  data = cpp,
  algorithms = algos, 
  verbose = log,

  initial_data_size = training_set_size / 2,
  max_iterations = nb_iter,
  mini_batch_size = (training_set_size / 2) / nb_iter
)


preds <- sampledata(osl, newdata = cpp, randomVariables, plot = TRUE)
preds
preds$denormalized$osl.estimator


generate_formulae <- function(W, A, Y){
  # Generate W Formulae
  W_form <- lapply(W, function(w) {
    s <- w
    first <- TRUE
    for (w_in in c(W)) {
      if(w_in == w) next
      if(first) s <- paste(s, w_in, sep = ' ~ ')
      else s <- paste(s, w_in, sep = ' + ')
      first <- FALSE
    }
    first <- TRUE
    formula(s)
  })
  names(W_form) <- W

  A_form <- lapply(A, function(a) {
    s <- a
    first <- TRUE
    for (a_in in c(W)) {
      if(first) s <- paste(s, a_in, sep = ' ~ ')
      else s <- paste(s, a_in, sep = ' + ')
      first <- FALSE
    }
    first <- TRUE
    formula(s)
  })
  names(A_form) <- A

  Y_form <- lapply(Y, function(y) {
    s <- y
    first <- TRUE
    for (y_in in c(W, A)) {
      if(first) s <- paste(s, y_in, sep = ' ~ ')
      else s <- paste(s, y_in, sep = ' + ')
      first <- FALSE
    }
    first <- TRUE
    formula(s)
  })
  names(Y_form) <- Y

  list(W=W_form, A=A_form, Y=Y_form)
}

