# This is just a working documnet for now. No TMLE is included at all!

# Step 1. MC approximation of the efficient influence curve.

# The process of Monte-Carlo approximating the efficient influence curve consists of two main steps: \rom{1} compute the
# so-called `\emph{h}-ratios', and \rom{2} compute a number of conditional expectations. This procedure is quite a
# mathematical endavour and we will not go into detail in how to derive the efficient influence curve. For this we
# redirect the reader to~\cite{VanderLaan2017}.

# Computing the H ratios:

# $h^*_{c_y(s)}(c_y) / h_{c_y}(c_y)$.
# In which $c_y = C_y(t)$, and $y = Y(t)$

# #1 Simple / fast / inefficient solution

get_h_ratios = function(B, N, intervention, data, randomVariables) {

  # Using $B$ we uniformely sample independently a number of integers from $1,\ldots, N$.
  Tsample <- sample.int(N, B, replace = TRUE)

  # Then we sample $B$ observations from $P^N$ and $B$ observations from $P^N_{s,a}.
  Osample <- foreach(i=seq(B), .combine=rbind) %dopar% {
    current_T <- Tsample[i]
    current <- osl$sample_iteratively(data = data[1,],
                                      randomVariables = randomVariables,
                                      # TODO: We should transform all variables back to their original value
                                      variable_of_interest = variable_of_interest,
                                      tau = current_T,
                                      return_full = TRUE)

    cbind(current[length(current), ], delta = 1)
  }

  # let s be a value $1 \le s \le \tau$
  Osample.Pstar <- foreach(i=seq(B), .combine=rbind) %dopar% {
    current <- osl$sample_iteratively(data = data[1,],
                                      randomVariables = randomVariables,
                                      intervention = intervention,
                                      variable_of_interest = variable_of_interest,
                                      tau = intervention$when)

    cbind(current[length(current), ], delta = 0)
  }

  Osample.full <- rbind(Osample, Osample.Pstar)

  h_ratio_predictors <- lapply(randomVariables, function(rv) {
                                 formula <- paste(rv$getY, '~', paste(rv$getX, collapse = '+'))
                                 glm(formula, Osample.full, family = binomial())
                                      })

  h_ratio_predictors
}

evaluation_of_conditional_expectations = function(intervention) {
  # for s < tau

  # Isn't the g*(r) always 1 in our case? (the probability of a treatment at the time of the treatment?)
  Z_score <- 1
  Z_score <- Z_score / osl$predict(data, A, all_estimators = FALSE, discrete = TRUE, continuous = FALSE)
}

tmle = function() {
  # Let $B$ be a large integer (the larger $B$ the better the approxmation).
  B = 1e5

  # Let $N$ be the number of observations.
  N = 90

  # We define our intervention as follows:
  intervention <- list(variable = 'A', when = c(2), what = c(1))

  #1 calculate H-ratios
  h_ratios <- get_h_ratios(B, N, intervention)

  #2 Solve expectaions
  solved_espectations <- evaluation_of_conditional_expectations(intervention)

}

