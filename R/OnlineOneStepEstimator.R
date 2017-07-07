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
get_h_ratios = function(osl, B, N, tau, intervention, data, randomVariables) {
  O_0 = data[1,]
  # In the second version we can use all observations drawn from P^N. In this case we sample $B$ observations $O^N$ and
  # extract from each of these observations the summary measures $C$. We first sample $B$ observations from $P^N$ and
  # then $B$ observations from $P^N_{s,a}. The size of Osample will be $BN$. After we have sampled these $BN$
  # observations, we augment the data with a $\delta$ column, which is either $1$ (when the data was sampled from P^N),
  # or $0$, whenever the data was sampled from $P^N_{a,s}$. Using these fully augmented data frames we now create
  # $3\tau$ machine learning estimators.

  # Only process the unique random variables
  formulae <- lapply(randomVariables, function(rv) {
    formula <- get_formula(rv)
  }) %>% unique

  browser()
  Osample_p <- foreach(b = seq(B), .combine = rbind) %dopar% {
    current <- osl$sample_iteratively(data = O_0,
                                      randomVariables = randomVariables,
                                      # TODO: !!!!We should transform all variables back to their original value?!!!!
                                      variable_of_interest = variable_of_interest,
                                      tau = N,
                                      return_type = 'summary_measures')

    # We store each observation with the correct delta
    cbind(current, delta = rep(1, length(current)))
  }

  # The final result is a list of estimators, which contains a GLM for each $C_W$, $C_A$, and $C_Y$, for each s in tau.
  # (so 3tau estimators)
  lapply(seq(tau), function(s) {
    # Because we use $BN$ observations in the previous sampling step, we should also draw BN observations from P^N_{s,a}.
    Osample_p_star <- foreach(b=seq(B * N), .combine=rbind) %dopar% {
      current <- osl$sample_iteratively(data = O_0,
                                        randomVariables = randomVariables,
                                        intervention = intervention,
                                        # TODO: !!!!We should transform all variables back to their original value?!!!!
                                        variable_of_interest = variable_of_interest,
                                        tau = s,
                                        return_type = 'summary_measures')

      cbind(current, delta = rep(0, length(current)))
    }
    # Kind of inefficient. Use data.tables here.
    Osample_p_full <- rbind(Osample_p, Osample_p_star)

    # Currently we use GLM here, but we should make use of a SuperLearner here.
    h_ratio_predictors <- lapply(formulae, function(formula) { glm(formula, Osample_p_full, family = binomial()) })

    # Store the names of the formulae, so we can index them easily later on
    names(h_ratio_predictors) <- formulae
    h_ratio_predictors
  })
}

evaluation_of_conditional_expectations = function(osl, B, N, h_ratio_predictors, variable_of_interest, randomVariables, data, tau, intervention) {
  # We have to create the conditional expectations using each of the random variables as a start point
  for (rv_id in seq_along(randomVariables)) {
    next_rv_id <- (rv_id %% length(randomVariables)) + 1

    rv <- randomVariables[[rv_id]]
    next_rv <- randomVariables[[next_rv_id]]

    # We are actually in next time block if the modulo was applied. This should be reflected in the time s.
    s_offset <- ifelse(rv_id > next_rv_id, 1, 0)

    # Get the formula so we can retrieve the prediction mechanism.
    formula <- get_formula(rv)

    for (t in seq(N)) {
      # First we have to estimate the h_ratio based on the C_rv
      # The h_ratio is the same for each t, wo we only need to estimate it once.
      h_ratio <- predict(h_ratio_predictors[[formula]], data[t,], type='response')

      # Then, we start on a given s
      for (s in seq(tau)) {
        # Now we have to sample from the conditional distribution of rv + 1. I.e., we have the our value for RV, and its
        # corresponding C. Using these values we can sample the next variable in the sequence. We do this until tau - s
        # because we want to start at time s (we actually start on time t and don't let it go further than tau - s).
        o_sample_conditional <- foreach(b=seq(B), .combine=rbind) %dopar% {
          # So, instead of moving s closer to tau, we move tau closer to s
          tau_cur <- tau - (s + s_offset)
          current <- osl$sample_iteratively(data = data[t,],
                                            randomVariables = randomVariables,
                                            start_from = next_rv,
                                            intervention = intervention,
                                            variable_of_interest = variable_of_interest,
                                            tau = tau_cur,
                                            return_type = 'outcome')[tau_cur, variable_of_interest$getY, with=FALSE]
        } %>%
          unlist %>%
          mean

        o_sample_marginal <- foreach(b=seq(B), .combine=rbind) %dopar% {
          tau_cur <- tau - (s + s_offset)
          current <- osl$sample_iteratively(data = data[t,],
                                            randomVariables = randomVariables,
                                            start = rv,
                                            intervention = intervention,
                                            variable_of_interest = variable_of_interest,
                                            tau = tau_cur,
                                            return_type = 'outcome')[tau_cur, variable_of_interest$getY, with=FALSE]
        } %>%
          unlist %>%
          mean

        h_ratio * (Oo_sample_conditional - o_sample_marginal)
      }
    }
  }
}

perform_initial_estimation = function(osl, B, data, randomVariables, intervention, variable_of_interest, tau) {
  foreach(i=seq(B), .combine=rbind) %do% {
    osl$sample_iteratively(data = data[1,],
                            randomVariables = randomVariables,
                            intervention = intervention,
                            variable_of_interest = variable_of_interest,
                            tau = tau)[tau, variable_of_interest$getY, with=FALSE]
  } %>%
    unlist %>% 
    mean
}

OnlineOneStepEstimator.perform = function(osl, randomVariables, data, variable_of_interest, intervention, tau, B = 1e5) {

  # Let $B$ be a large integer (the larger $B$ the better the approxmation),
  # let $N$ be the number of observations,
  N = nrow(data)
  # let intervention be the intervention we whish to oppose on the system, and
  # let tau the the outcome of interest. 

  #1 Before anything, perform our initial estimation of our parameter of interest
  #initial_estimate <- perform_initial_estimation(osl = osl,
                                                 #B = B,
                                                 #data = data,
                                                 #randomVariables = randomVariables,
                                                 #intervention = intervention,
                                                 #variable_of_interest = variable_of_interest,
                                                 #tau = tau)

  #2 calculate H-ratios
  h_ratio_predictors <- get_h_ratios(osl = osl, B = B, N = N, tau = tau, intervention = intervention, data = data, 
                           randomVariables = randomVariables )

  #3 Solve expectaions
  # We want to approximate the conditional expectation from RV (start) till Ytau
  D_star_evaluation = evaluation_of_conditional_expectations(B, N, h_ratio_predictors = h_ratio_predictors,
                                         variable_of_interest = variable_of_interest, 
                                         randomVariables = randomVariables,
                                         data = data,
                                         tau = tau,
                                         intervention = intervention)

  #4 Add the EIC term to the predictor to make it well behaved
  # initial_estimate + D_star_evaluation

  #5 Calculate the variance of the estimator
  # see chapter antoine

}

