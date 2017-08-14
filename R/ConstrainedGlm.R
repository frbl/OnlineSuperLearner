ConstrainedGlm.fit <- function(formula, delta, ...) {
  ncovariates <- length(labels(terms(abc)))
  my_delta <- delta
  bounded_logit <- function(delta = my_delta) {
    structure(
      list(## mu mapsto logit( [mu - delta]/[1 - 2 delta]  ).
        linkfun = function(mu) {
          qlogis((mu-delta)/(1-2*delta))
        },

        ## eta mapsto delta + (1 - 2 delta) expit (eta).
        linkinv = function(eta) {
          delta + (1-2*delta)*plogis(eta)
        },


        ## derivative of inverse link wrt eta
        mu.eta = function(eta) {
          expit.eta <- plogis(eta)
          (1-2*delta)*expit.eta*(1-expit.eta)
        },


        ## test of validity for eta
        valideta = function(...) TRUE,
        name = 'bounded-logit'
      ),
      class = "link-glm"
    )
  }

  ## Fit the constrained regression
  bd_logit <- bounded_logit()
  return(glm(formula= formula, family = binomial(link = bd_logit), start = rep(0, ncovariates), ...))
}
