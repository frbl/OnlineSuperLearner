#' Constrained logistic regression
#' In this function we create a regression for which the predicted probabilities are contstrained. That is, they can not be
#' less than a minimum of delta, or a maxiumum of 1 - delta.
#' @param formula the formula to use for the regression
#' @param delta the threshold used for constraining the probabilities
#' @param ... the other arguments passed to GLM
#' @return a fitted glm
#' @importFrom stats glm
#' @export
ConstrainedGlm.fit <- function(formula, delta, data, ...) {
  ## 1 for intercept
  ncovariates <- formula %>% 
    terms %>%
    labels %>%
    length %>%
    add(.,1)
  
  bounded_logit <- function(delta) {
    structure(
      list(## mu mapsto logit( [mu - delta]/[1 - 2 delta]  ).
        linkfun = function(mu) {
          print(mu)
          logit((mu-delta)/(1-2*delta))
        },

        ## eta mapsto delta + (1 - 2 delta) expit (eta).
        linkinv = function(eta) {
          delta + ((1-2*delta)*expit(eta))
        },

        ## derivative of inverse link wrt eta
        mu.eta = function(eta) {
          expit.eta <- expit(eta)
          (1-2*delta)*expit.eta*(1-expit.eta)
        },
        ## test of validity for eta
        valideta = function(...) TRUE,
        validmu = function(...) TRUE,
        name = 'bounded-logit'
      ),
      class = "link-glm"
    )
  }

    ## "deviance residuals" as a function of eta

  ## Fit the constrained regression
  bd_logit <- bounded_logit(delta)
  #browser()
  family = binomial(link = bd_logit)

  ## Override the residuals function
  family$dev.resids <- function(y, eta, wt) {
    mu <- bd_logit$linkinv(eta)
    wt*(y/mu + (1-y)/(1-mu))
  }

  return(glm(formula = formula, family = family, data=data,
             control = list(maxit=10000), start = rep(1/2, 2), ...))

}
