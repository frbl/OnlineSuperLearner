
## This is just a working documnet for now. No TMLE is included at all!

## Step 1. MC approximation of the efficient influence curve.

## The process of Monte-Carlo approximating the efficient influence curve consists of two main steps: \rom{1} compute the
## so-called `\emph{h}-ratios', and \rom{2} compute a number of conditional expectations. This procedure is quite a
## mathematical endavour and we will not go into detail in how to derive the efficient influence curve. For this we
## redirect the reader to~\cite{VanderLaan2017}.

## Computing the H ratios:

## $h^*_{c_y(s)}(c_y) / h_{c_y}(c_y)$.
## In which $c_y = C_y(t)$, and $y = Y(t)$

## #1 Simple / fast / inefficient solution
##get_h_ratio_estimators = function(B, N, intervention, data, randomVariables) {

  ### Using $B$ we uniformely sample independently a number of integers from $\{1,\ldots, N\}$.
  ##Tsample <- sample.int(N, B, replace = TRUE)

  ### Then we sample $B$ observations from $P^N$ and $B$ observations from $P^N_{s,a}.
  ##Osample <- foreach(b=seq(B), .combine=rbind) %dopar% {
    ##current_T <- Tsample[b]
    ##current <- osl$sample_iteratively(data = data[1,],
                                      ##randomVariables = randomVariables,
                                      ### TODO: We should transform all variables back to their original value
                                      ##variable_of_interest = variable_of_interest,
                                      ##tau = current_T,
                                      ##return_type = 'summary_measures')

    ##cbind(current[length(current), ], delta = 1)
  ##}

  ### let s be a value $1 \le s \le \tau$
  ##Osample.Pstar <- foreach(b=seq(B), .combine=rbind) %dopar% {
    ##current <- osl$sample_iteratively(data = data[1,],
                                      ##randomVariables = randomVariables,
                                      ##intervention = intervention,
                                      ##variable_of_interest = variable_of_interest,
                                      ##tau = intervention$when,
                                      ##return_type = 'summary_measures')

    ##cbind(current[length(current), ], delta = 0)
  ##}

  ##Osample.full <- rbind(Osample, Osample.Pstar)

  ##h_ratio_predictors <- lapply(randomVariables, function(rv) {
                                 ##formula <- paste(rv$getY, '~', paste(rv$getX, collapse = '+'))
                                 ##glm(formula, Osample.full, family = binomial())
                                      ##})

  ##h_ratio_predictors
##}

##' OneStepEstimator
##'
##' @docType class
##' @importFrom R6 R6Class
#OneStepEstimator <- R6Class("OneStepEstimator",
  #public =
    #list(
      #initialize = function() { },

      #perform = function(osl, initial_estimate, randomVariables, data, variable_of_interest, intervention, tau, B = 1e5) {
        ## Let $B$ be a large integer (the larger $B$ the better the approxmation),
        ## let $N$ be the number of observations,
        #N = nrow(data)
        ## let intervention be the intervention we whish to oppose on the system, and
        ## let tau the the outcome of interest. 

        ##1 Before anything, perform our initial estimation of our parameter of interest
        #if(is.null(initial_estimate)) {
          #initial_estimate <- perform_initial_estimation(osl = osl,
                                                        #B = B,
                                                        #data = data,
                                                        #randomVariables = randomVariables,
                                                        #intervention = intervention,
                                                        #variable_of_interest = variable_of_interest,
                                                        #tau = tau)
        #}

        ##2 calculate H-ratios
        #h_ratio_predictors <- get_h_ratio_estimators(osl = osl, 
                                                     #B = B, 
                                                     #N = N,
                                                     #tau = tau,
                                                     #intervention = intervention,
                                                     #data = data,
                                                     #randomVariables = randomVariables )

        ##3 Solve expectaions
        ## We want to approximate the conditional expectation from RV (start) till Ytau
        #D_star_evaluation = evaluation_of_conditional_expectations(osl = osl,
                                                                  #B = B,
                                                                  #N = N,
                                                                  #h_ratio_predictors = h_ratio_predictors,
                                                                  #variable_of_interest = variable_of_interest, 
                                                                  #randomVariables = randomVariables,
                                                                  #data = data,
                                                                  #tau = tau,
                                                                  #intervention = intervention)
        #print(D_star_evaluation)

        ## Calculate the new estimate
        #oos_estimate <- D_star_evaluation %>% unlist %>% sum
        #oos_estimate <- oos_estimate + initial_estimate
        #if(is.nan(oos_estimate)) {
          #warning('Oos estimate is NaN, setting to zero')
          #oos_estimate <- 0
        #}
        #oos_variance <- 0

        #list(
          #oos_estimate = oos_estimate,
          #oos_variance = oos_variance
        #)

      #},

      #get_h_ratio_estimators = function(osl, B, N, tau, intervention, data, randomVariables) {
        #O_0 = copy(data[1,])
        ## We first sample $B$ observations from $P^N$ (that is, N blocks of
        ## summary relevant history) and then $BN$ observations from $P^N_{s,a}
        ## (that is, $B * N * \tau$ blocks$ of relevant history).
        ## The size of Osample will be $B N$. 
        ##
        ## After we have sampled these $BN$ observations, we augment the data
        ## with a $\delta$ column, which is either $1$ (when the data was
        ## sampled from P^N), or $0$, whenever the data was sampled from
        ## $P^N_{a,s}$. Using these fully augmented data frames we now create
        ## $3$ (one for each W,A,Y) times $\tau$ machine learning estimators. 
        #formulae <- lapply(randomVariables, function(rv) {
          #formula <- rv$get_formula_string(Y = 'delta')
        #}) %>% unique

        #print('Starting sampling from PN')
        #tic <- Sys.time()
        #Osample_p <- foreach(b = seq(B), .combine = rbind) %dopar% {
          ## TODO: Note that the summary measures we currently collect are
          ## NORMALIZED. I think that this does not matter for
          ## calculating the h-ratios, but we need to check this.

          ## TODO: Note that we are sampling always starting from O_0.
          #cat('Iteration ', b, '\n')
          #osl$sample_iteratively(data = O_0,
                                 #randomVariables = randomVariables,
                                 #tau = N,
                                 #return_type = 'full')
        #}

        ## We store each observation with the correct delta
        #Osample_p <- cbind(Osample_p, delta = rep(1, length(Osample_p)))

        #toc <- Sys.time()
        #cat('Sampling ', B,' observations from P^N took ', (toc - tic), ' seconds.')

        #tic <- Sys.time()

        ## Because we use $BN$ observations in the previous sampling step, we should also draw BN observations from P^N_{s,a}.
        #Osample_p_star <- foreach(b=seq(B * N), .combine=rbind) %dopar% {
          #current <- osl$sample_iteratively(data = O_0,
                                            #randomVariables = randomVariables,
                                            #intervention = intervention,
                                            #tau = tau,
                                            #return_type = 'full')

        #}
        #toc <- Sys.time()
        #cat('Sampling ', B*N,' observations from PN* took ', (toc - tic), ' seconds.')


        ## Add an S column to the data, so we know which summary measure belongs to which s
        #time_s_column <- lapply(seq(N*B), function(i) ((i - 1) %% tau) + 1)  %>% unlist

        ## Kind of inefficient. Use data.tables here.
        #Osample_p_star <- cbind(Osample_p_star, delta = rep(0, nrow(Osample_p_star)), time_s_column = time_s_column)

        ## Currently we use GLM here, but we should make use of a SuperLearner here.
        #h_ratio_predictors_per_s <- lapply(seq(tau), function(time_s) {
          #Osample_p_full <- rbind(Osample_p, Osample_p_star[time_s_column == 1][,!'time_s_column'])
          #h_ratio_predictors <- lapply(formulae, function(formula) { glm(formula, Osample_p_full, family = binomial()) })
          ## Store the names of the formulae, so we can index them easily later on
          #names(h_ratio_predictors) <- formulae
          #h_ratio_predictors
        #})

        ## The final result is a list of estimators, which contains a GLM for each $C_W$, $C_A$, and $C_Y$, for each s in tau.
        ## (so 3tau estimators)
        #h_ratio_predictors_per_s
      #},

      #evaluation_of_conditional_expectations = function(osl, B, N, h_ratio_predictors, variable_of_interest, randomVariables, data, tau, intervention) {
        ## We have to create the conditional expectations using each of the random variables as a start point
        #influence_curve_for_each_rv <- c()
        #for (rv_id in seq_along(randomVariables)) {
          #next_rv_id <- (rv_id %% length(randomVariables)) + 1

          #rv <- randomVariables[[rv_id]]
          #next_rv <- randomVariables[[next_rv_id]]

          #print(rv$getY)
          ##if(rv$getY == 'Y') browser()
          #current_rv <- rv$getY
          #var_of_interest <- variable_of_interest$getY

          ## We are actually in next time block if the modulo was applied. This should be reflected in the time s.
          #s_offset <- ifelse(rv_id > next_rv_id, 1, 0)

          ## Get the formula so we can retrieve the prediction mechanism.
          #formula <- rv$get_formula_string(Y='delta')

          #efficient_influence_curve_for_rv <- foreach(t=seq(N), .combine='sum') %dopar% {
            #print(t)
            #dat <- copy(data[t,])

            ## Then, we start on a given s
            #difference_in_expectations_for_all_s <- foreach(s=seq(tau), .combine='sum') %do% {
              ## First we have to estimate the h_ratio based on the C_rv
              ## The h_ratio is the same for each t, wo we only need to estimate it once.
              #h_ratio <- predict(h_ratio_predictors[[s]][[formula]], data[t,], type='response') %>% as.numeric
              #h_ratio <- h_ratio / (1.0 - h_ratio)

              ## Now we have to sample from the conditional distribution of rv + 1. I.e., we have the our value for RV, and its
              ## corresponding C. Using these values we can sample the next variable in the sequence. We do this until tau - s
              ## because we want to start at time s (we actually start on time t and don't let it go further than tau - s).

              ## So, instead of moving s closer to tau, we move tau closer to s
              ## TODO: deNormalize the output here
              #tau_cur <- tau - ((s - 1) + s_offset)

              ## If tau ends up to be 0, and we already know y(tau), we can just get
              ## it from the data. Expectation of a constant is the constant itself,
              ## right?
              #if(tau_cur == 0 && current_rv == var_of_interest) {
                #o_sample_marginal <- as.numeric(dat[,var_of_interest, with=FALSE])
              #} else {
                #o_sample_conditional <- foreach(b=seq(B), .combine=rbind) %do% {
                  #current <- osl$sample_iteratively(data = dat,
                                                    #randomVariables = randomVariables,
                                                    #start_from = next_rv,
                                                    #intervention = intervention,
                                                    #tau = tau_cur,
                                                    #return_type = 'observations')[tau_cur, var_of_interest, with=FALSE]
              #} %>%
                #unlist %>%
                #mean
              #}

              #tau_cur <- tau - (s - 1)
              #o_sample_marginal <- foreach(b=seq(B), .combine=rbind) %do% {
                #current <- osl$sample_iteratively(data = data[t,],
                                                  #randomVariables = randomVariables,
                                                  #start = rv,
                                                  #intervention = intervention,
                                                  #tau = tau_cur,
                                                  #return_type = 'observations')[tau_cur, var_of_interest, with=FALSE]
              #} %>%
                #unlist %>%
                #mean

              #h_ratio * (o_sample_conditional - o_sample_marginal)
            #}
            #difference_in_expectations_for_all_s / N
          #}
          #influence_curve_for_each_rv <- c(influence_curve_for_each_rv, efficient_influence_curve_for_rv)
        #}
        #influence_curve_for_each_rv
      #},

      #perform_initial_estimation = function(osl, B, data, randomVariables, intervention, variable_of_interest, tau) {
        #foreach(i=seq(B), .combine=rbind) %do% {
          #osl$sample_iteratively(data = data[1,],
                                  #randomVariables = randomVariables,
                                  #intervention = intervention,
                                  #variable_of_interest = variable_of_interest,
                                  #tau = tau)[tau, variable_of_interest$getY, with=FALSE]
        #} %>%
          #unlist %>% 
          #mean
      #}
    #),
  #active =
    #list(
    #),
  #private =
    #list(
    #)
#)


## #2 More efficient but slow solution

