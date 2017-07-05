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
#get_h_ratios = function(B, N, intervention, data, randomVariables) {

  ## Using $B$ we uniformely sample independently a number of integers from $\{1,\ldots, N\}$.
  #Tsample <- sample.int(N, B, replace = TRUE)

  ## Then we sample $B$ observations from $P^N$ and $B$ observations from $P^N_{s,a}.
  #Osample <- foreach(b=seq(B), .combine=rbind) %dopar% {
    #current_T <- Tsample[b]
    #current <- osl$sample_iteratively(data = data[1,],
                                      #randomVariables = randomVariables,
                                      ## TODO: We should transform all variables back to their original value
                                      #variable_of_interest = variable_of_interest,
                                      #tau = current_T,
                                      #return_type = 'summary_measures')

    #cbind(current[length(current), ], delta = 1)
  #}

  ## let s be a value $1 \le s \le \tau$
  #Osample.Pstar <- foreach(b=seq(B), .combine=rbind) %dopar% {
    #current <- osl$sample_iteratively(data = data[1,],
                                      #randomVariables = randomVariables,
                                      #intervention = intervention,
                                      #variable_of_interest = variable_of_interest,
                                      #tau = intervention$when,
                                      #return_type = 'summary_measures')

    #cbind(current[length(current), ], delta = 0)
  #}

  #Osample.full <- rbind(Osample, Osample.Pstar)

  #h_ratio_predictors <- lapply(randomVariables, function(rv) {
                                 #formula <- paste(rv$getY, '~', paste(rv$getX, collapse = '+'))
                                 #glm(formula, Osample.full, family = binomial())
                                      #})

  #h_ratio_predictors
#}

get_formula = function(rv) {
  paste('delta ~', paste(sort(rv$getX), collapse = ' + '))
}

# #2 More efficient but slow solution
get_h_ratios_second = function(B, N, tau, intervention, data, randomVariables) {
  O_0 = data[1,]
  # In the second version we can use all observations drawn from P^N. In this case we sample $B$ observations $O^N$ and
  # extract from each of these observations the summary measures $C$.

  # We first sample $B$ observations from $P^N$ and $B$ observations from $P^N_{s,a}. The size of Osample will be $BN$.
  Osample <- foreach(b=seq(B), .combine=rbind) %dopar% {
    current <- osl$sample_iteratively(data = O_0,
                                      randomVariables = randomVariables,
                                      # TODO: !!!!We should transform all variables back to their original value?!!!!
                                      variable_of_interest = variable_of_interest,
                                      tau = N,
                                      return_type = 'summary_measures')

    # We store each observation with the correct delta
    cbind(current, delta = rep(1, length(current)))
  }

  # Because we use $BN$ observations in the previous sampling step, we should also draw BN observations from P^N_{s,a}.
  h_ratio_predictors <- lapply(seq(tau), function(s) {
    Osample_full_for_s <- foreach(b=seq(B), .combine=rbind) %dopar% {
      current <- osl$sample_iteratively(data = O_0,
                                        randomVariables = randomVariables,
                                        intervention = intervention,
                                        # TODO: !!!!We should transform all variables back to their original value?!!!!
                                        variable_of_interest = variable_of_interest,
                                        tau = s,
                                        return_type = 'summary_measures')

      cbind(current, delta = rep(0, length(current)))
    } %>% rbind(Osample, .)

    # Only process the unique random variables
    formulae <- lapply(randomVariables, function(rv) {
      formula <- get_formula(rv)
    }) %>% unique

    result <- lapply(formulae, function(formula) {
      glm(formula, Osample_full_for_s, family = binomial())
    })

    # Store the names of the formulae, so we can index them easily later on
    names(result) <- formulae
  })

  # The final result is a list of lists. The outer list goes from 1 to tau. The inner list contains a GLM for each
  # $C_W$, $C_A$, and $C_Y$. (so tau * 3 elements)
  h_ratio_predictors
}

evaluation_of_conditional_expectations = function(B, N, h_ratios, variable_of_interest, randomVariables, data, tau, intervention) {
  # We have to create the conditional expectations using each of the random variables as a start point
  for (rv in randomVariables) {
    formula <- get_formula(rv)
    # Then, we start on a given s
    for (s in seq(tau)) {

      # The h_ratio is the same for each s, wo we only need to estimate it once.
      h_ratio <- predict(h_ratios[[s]][[formula]])
      for (t in seq(N)) {
        # Now we have to sample from the conditional distribution of rv + 1. I.e., we have the our value for RV, and its
        # corresponding C. Using these values we can sample the next variable in the sequence. We do this until tau - s
        # because we want to start at time s (we actually start on time t and don't let it go further than tau - s).
        Osample_conditional <- foreach(b=seq(B), .combine=rbind) %dopar% {
          current <- osl$sample_iteratively(data = data[t,],
                                            randomVariables = randomVariables,
                                            start = rv,
                                            intervention = intervention,
                                            variable_of_interest = variable_of_interest,
                                            tau = tau - s,
                                            return_type = 'outcome')
        } %>% colmeans

        Osample_marginal <- foreach(b=seq(B), .combine=rbind) %dopar% {
          #current <- osl$sample_iteratively(data = data[t,],
                                            #randomVariables = randomVariables,
                                            #start = rv,
                                            #intervention = intervention,
                                            #variable_of_interest = variable_of_interest,
                                            #tau = tau - s,
                                            #return_type = 'outcome')
        } %>% colmeans

        h_ratio * (Osample_conditional - Osample_marginal)
      }
    }
  }
}

tmle = function() {
  data = 'XXXXXX'
  randomVariables = 'XXXX'
  Y = 'XXXX'

  # Let $B$ be a large integer (the larger $B$ the better the approxmation).
  B = 1e5

  # Let $N$ be the number of observations.
  N = nrow(data)

  # We define our intervention as follows:
  intervention <- list(variable = 'A', when = c(2), what = c(1))

  # We want to approximate the outcome at time tau.
  tau = intervention$when + 1

  #1 calculate H-ratios
  h_ratios <- get_h_ratios(B = B, N = N, tau = tau, intervention = intervention, data = data, 
                           randomVariables = randomVariables )

  #2 Solve expectaions
  # We want to approximate the conditional expectation from RV (start) till Ytau
  evaluation_of_conditional_expectations(B, N, h_ratios = h_ratios,
                                         variable_of_interest = Y, 
                                         randomVariables = randomVariables,
                                         data = data,
                                         tau = tau,
                                         intervention = intervention)

  #3
  #4
  #5


}

