#' OneStepEstimator
#'
#' The one step estimator (OOS) is a technique that improves our initial estimates of the parameter of interest and targets
#' them towards this parameter of interest. In order to use the OOS, one has to solve the Efficient Influence Curve
#' equation, which can be done using a Monte-Carlo approximation.  The process of Monte-Carlo approximating the
#' efficient influence curve consists of two main steps: (i) compute the so-called `\emph{h}-ratios', and (ii) compute a
#' number of conditional expectations. This procedure is implemented in this class and can be started by calling the
#' \core{perform} method of an instance of this class. 
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(osl, randomVariables, N, B, pre_processor, discrete = TRUE) }}{ 
#'     Initializes the online one step estimator. It uses an earlier fitted online super learner to sample from the
#'     conditional densities.
#'     @param osl the online superlearner, which was fitted earlier on the data
#'     @param randomVariables a list of random variables used for fitting the OSL
#'     @param N the number of measurements in a timeseries
#'     @param B the number of iterations we should do while sampling from the conditional expectations
#'     @param pre_processor the \core{PreProcessor} object used to normalize the data.
#'     @param discrete = TRUE whether we should use the discrete (true) or continuous (false) super learner 
#'   } 
#' 
#'   \item{\code{perform(initial_estimate, data, variable_of_interest, intervention}}{ 
#'     This method actually runs the oos. Based on an initial estimate, it calculates an update term to add to this
#'     estimate. This will make the estimator well behaved (i.e., normally distributed). The function will add this
#'     correction term to the initial estimate and return the estimated variance of the estimator.
#'     @param initial_estimate the initial estimate of the target parameter, as calculated using OSL
#'     @param data the data to seed the sampling procedure
#'     @param variable_of_interest the variable we are interested in (e.g., the Y random variable)
#'     @param intervention the intervention we want to perform. See \code{InterventionParser} for more details
#'     @return a list containing two elements: \code{oos_estimate} and \code{oos_variance}. This first element (\code{oos_estimate})
#'     contains the updated estimate of the target parameter. The second element (\code{oos_variance}), contains the
#'     variance of this estimator, which can be used to derive confidence intervals.
#'   } 
#' 
#'   \item{\code{get_h_ratio_estimators(tau, intervention, data) }}{ 
#'     This method can be used to perform the first step of OOS. It can be used to retrieve a list of h-ratio
#'     estimators. This method returns an estimator for each random variable (W, A, and Y), and for each time s from 1
#'     to tau. This method uses the more efficient way, as described in Van der Laan 2017.
#'     @param tau the time at which we want to measure the effect of an intervention.
#'     @param intervention the intervention itself, see \code{InterventionParser} for more details
#'     @param data the data to seed the sampling procedure and thereby h-ratio predictor fitting
#'     @return a list of lists of estimators. The outer list has a list for each s 1 to tau. For each entry in this
#'     list, we have another list for each outcome measure, as indexed using their prediction formulae.
#'   } 
#' 
#'   \item{\code{evaluation_of_conditional_expectations(h_ratio_predictors, variable_of_interest, data, tau}}{ 
#'     In this function one can perform the second step of OOS, calculate the conditional expectations / difference in
#'     conditional expectations. 
#'     @param h_ratio_predictors the list of predictors, in a format returned using the \code{get_h_ratio_estimators}
#'     format.
#'     @param variable_of_interest the variable we are currently interested in (the outcome variable, Y).
#'     @param data the data to seed the sampling procedure and thereby conditional expectation evaluation. 
#'     @param tau the time till which we have to sample.
#'   } 
#' 
#'   \item{\code{perform_initial_estimation(data, intervention, tau) }}{ 
#'     This function can be used to generate an initial estimation, calculated using the plain OSL. This method then
#'     returns a value given the provided data, tau, and intervention.
#'     @param data the data to seed the sampling procedure.
#'     @param intervention the intervention itself, see \code{InterventionParser} for more details
#'     @param tau the time at which we want to evaluate the intervention
#'   } 
#' 
#' }  
#' @docType class
#' @include ConstrainedGlm.R
#' @importFrom R6 R6Class
#' @importFrom speedglm speedlm updateWithMoreData
OneStepEstimator <- R6Class("OneStepEstimator",
  public =
    list(
      initialize = function(osl, randomVariables, N, B, pre_processor, discrete = TRUE) {
        private$osl <- Arguments$getInstanceOf(osl, 'OnlineSuperLearner')

        private$N <- N#Arguments$getInteger(N, c(1, Inf))
        private$B <- B#Arguments$getIngeger(B, c(1, Inf))
        private$discrete <- Arguments$getLogical(discrete)
        private$randomVariables <- Arguments$getInstanceOf(randomVariables, 'list')
        private$pre_processor <- pre_processor
      },

      perform = function(initial_estimate, data, variable_of_interest, intervention, tau) {
        # Let $B$ be a large integer (the larger $B$ the better the approxmation),
        # let $N$ be the number of observations,
        N = nrow(data)
        # let intervention be the intervention we whish to oppose on the system, and
        # let tau the the outcome of interest.

        #1 Before anything, perform our initial estimation of our parameter of interest
        if(is.null(initial_estimate)) {
          initial_estimate <- perform_initial_estimation(data = data,
                                                        intervention = intervention,
                                                        variable_of_interest = variable_of_interest,
                                                        tau = tau)
        }

        #2 calculate H-ratios
        h_ratio_predictors <- self$get_h_ratio_estimators(tau = tau,
                                                     intervention = intervention,
                                                     data = data)

        #3 Solve expectaions
        # We want to approximate the conditional expectation from RV (start) till Ytau
        D_star_evaluation = self$evaluation_of_conditional_expectations(h_ratio_predictors = h_ratio_predictors,
                                                                  variable_of_interest = variable_of_interest,
                                                                  data = data,
                                                                  tau = tau,
                                                                  intervention = intervention)

        print(D_star_evaluation)

        # Calculate the new estimate
        oos_estimate <- D_star_evaluation %>% unlist %>% sum
        oos_estimate <- oos_estimate + initial_estimate
        if(is.nan(oos_estimate)) {
          warning('Oos estimate is NaN, setting to zero')
          oos_estimate <- 0
        }
        oos_variance <- 0

        list(
          oos_estimate = oos_estimate,
          oos_variance = oos_variance
        )

      },

      get_h_ratio_estimators = function(tau, intervention, data) {
        # We first sample $B$ observations from $P^N$ (that is, N blocks of
        # summary relevant history) and then $BN$ observations from $P^N_{s,a}
        # (that is, $B * N * \tau$ blocks$ of relevant history).
        # The size of Osample will be $B N$.
        #
        # After we have sampled these $BN$ observations, we augment the data
        # with a $\delta$ column, which is either $1$ (when the data was
        # sampled from P^N), or $0$, whenever the data was sampled from
        # $P^N_{a,s}$. Using these fully augmented data frames we now create
        # $3$ (one for each W,A,Y) times $\tau$ machine learning estimators.
        formulae <- lapply(self$get_randomVariables, function(rv) {
          formula <- rv$get_formula_string(Y = 'delta')
        }) %>% unique

        print('Starting sampling from PN')
        tic <- Sys.time()
        Osample_p <- foreach(t = seq(self$get_N), .combine = rbind) %do% {
          O_0 = data[t,]
          
          foreach(b = seq(self$get_B), .combine = rbind) %dopar% {
            # TODO: Note that the summary measures we currently collect are
            # NORMALIZED. I think that this does not matter for
            # calculating the h-ratios, but we need to check this.

            # TODO: Note that we are sampling always starting from O_0.
            cat('Iteration ', b, '\n')
            current <- self$get_osl$sample_iteratively(data = O_0,
                                                      randomVariables = self$get_randomVariables,
                                                      tau = 1,
                                                      discrete = self$get_discrete,
                                                      intervention = NULL,
                                                      return_type = 'full')
            current
          }
        }

        # We store each observation with the correct delta
        Osample_p <- cbind(Osample_p, delta = rep(1, length(Osample_p)))

        toc <- Sys.time()
        cat('Sampling ', self$get_B,' observations from P^N took ', (toc - tic), ' seconds.')

        tic <- Sys.time()

        # Because we use $BN$ observations in the previous sampling step, we should also draw BN observations from P^N_{s,a}.

        Osample_p_star <- foreach(t = seq(self$get_N), .combine = rbind) %do% {
          O_0 = data[t,]
          
          foreach(b = seq(self$get_B), .combine = rbind) %dopar% {
            cat('Iteration ', b, '\n')
            current <- self$get_osl$sample_iteratively(data = O_0,
                                                      randomVariables = self$get_randomVariables,
                                                      tau = tau,
                                                      discrete = self$get_discrete,
                                                      intervention = intervention,
                                                      return_type = 'full')

          }
        }
        toc <- Sys.time()
        cat('Sampling ', self$get_B*self$get_N,' observations from PN* took ', (toc - tic), ' seconds.')


        # Add an S column to the data, so we know which summary measure belongs to which s
        time_s_column <- lapply(seq(self$get_N*self$get_B), function(i) ((i - 1) %% tau) + 1)  %>% unlist

        # Kind of inefficient. Use data.tables here.
        Osample_p_star <- cbind(Osample_p_star, delta = rep(0, nrow(Osample_p_star)), time_s_column = time_s_column)

        # Currently we use GLM here, but we should make use of a SuperLearner here.
        # We generate tau estimators here, one for each s to tau
        browser()
        h_ratio_predictors_per_s <- lapply(seq(tau), function(time_s) {

          Osample_p_full <- rbind(Osample_p, Osample_p_star[time_s_column == 1][,!'time_s_column'])

          h_ratio_predictors <- lapply(formulae, function(formula) {
            #speedglm::speedglm.wfit(formula, Osample_p_full, family = binomial(), method='Cholesky')
            hide_warning_convergence(ConstrainedGlm.fit(formula = formula(formula),
                                                        data = Osample_p_full,
                                                        delta = 0.05))
          })
          # Store the names of the formulae, so we can index them easily later on
          names(h_ratio_predictors) <- formulae
          h_ratio_predictors
        })

        # The final result is a list of estimators, which contains a GLM for
        # each $C_W$, $C_A$, and $C_Y$, for each s in tau.  (so 3tau
        # estimators)
        h_ratio_predictors_per_s
      },

      evaluation_of_conditional_expectations = function(h_ratio_predictors, variable_of_interest, data, tau, intervention) {
        # We have to create the conditional expectations using each of the
        # random variables as a start point
        influence_curve_for_each_rv <- c()

        var_of_interest <- variable_of_interest$getY

        for (rv_id in seq_along(self$get_randomVariables)) {
          current_rvs <- private$get_next_and_current_rv(rv_id)

          # This is the outer loop of the influence curve
          efficient_influence_curve_for_current_rv <- foreach(t=seq(self$get_N), .combine='sum') %dopar% {
            current_dat <- data[t,]

            # This is the inner loop of the influence curve

            difference_in_expectations_for_all_s <- foreach(s=seq(tau), .combine='sum') %do% {
              # Then, we start on a given s
              private$get_influence_curve_for_one_random_variable(s = s,
                                                                  tau = tau,
                                                                  intervention = intervention,
                                                                  dat = current_dat,
                                                                  h_ratio_predictors = h_ratio_predictors,
                                                                  var_of_interest = var_of_interest,
                                                                  current_rvs = current_rvs)


            }
            difference_in_expectations_for_all_s / self$get_N
          }
          influence_curve_for_each_rv <- c(influence_curve_for_each_rv,
                                           efficient_influence_curve_for_current_rv)
        }
        influence_curve_for_each_rv
      },


      perform_initial_estimation = function(data, intervention, tau) {
        foreach(i=seq(B), .combine=rbind) %do% {
          self$get_osl$sample_iteratively(data = data[1,],
                                          randomVariables = self$get_randomVariables,
                                          intervention = intervention,
                                          discrete = self$get_discrete,
                                          return_type = 'observations',
                                          tau = tau)[tau, variable_of_interest$getY, with=FALSE]
        } %>%
          unlist %>%
          mean
      },

      calculate_h_ratio = function(h_ratio_predictors, s, formula, data) {
        if(is.null(h_ratio_predictors)) {
          # This is just for testing purposes!
          warning('Test function was called!')
          return(0.5)
        }
        assert_that(!is.null(data))
        h_ratio <- predict(h_ratio_predictors[[s]][[formula]], newdata = data, type='response') %>% as.numeric
        result <- h_ratio / (1.0 - h_ratio)
        if((abs(result) > 1000)) {
          warning(paste('H-ratio is very high:', result, 'because predicted h was', h_ratio))
        }
        # Bounding the result on .95% (19)
        min(result, 20)
      }
    ),
  active =
    list(
      get_osl = function() {
        return(private$osl)
      },

      get_N = function() {
        return(private$N)
      },

      get_B = function() {
        return(private$B)
      },

      get_discrete = function() {
        return(private$discrete)
      },

      get_randomVariables = function() {
        return(private$randomVariables)
      },

      get_pre_processor = function() {
        return(private$pre_processor)
      }
    ),
  private =
    list(
      osl = NULL,
      N = NULL,
      B = NULL,
      randomVariables = NULL,
      discrete = NULL,
      pre_processor = NULL,

      get_next_and_current_rv = function(current_rv_index) {
        # Find the ID for the next random variable
        next_rv_id <- (current_rv_index %% length(self$get_randomVariables)) + 1
        rv <- self$get_randomVariables[[current_rv_index]]
        next_rv <- self$get_randomVariables[[next_rv_id]]

        # We are actually in next time block if the modulo was applied. This should be reflected in the time s.
        s_offset <- ifelse(current_rv_index > next_rv_id, 1, 0)
        list(rv = rv, next_rv = next_rv, s_offset = s_offset)
      },

      is_current_node_treatment = function(s, intervention, rv) {
        Y <- rv$getY
        if (intervention$variable != Y) return(FALSE)
        if (!(s %in% intervention$when)) return(FALSE)
        return(TRUE)
      },

      get_influence_curve_for_one_random_variable = function(s, tau, intervention, dat, h_ratio_predictors, current_rvs, current_rv_name, var_of_interest) {
        # Get the formula ant output name so we can retrieve the prediction mechanism.
        formula <- current_rvs$rv$get_formula_string(Y='delta')

        # First we have to estimate the h_ratio based on the C_rv, and the current s and random variable
        h_ratio <- self$calculate_h_ratio(h_ratio_predictors, s, formula, dat)

        # Now we have to sample from the conditional distribution of rv + 1.
        # I.e., we have the our value for RV, and its corresponding C. Using
        # these values we can sample the next variable in the sequence. We do
        # this until tau - s because we want to start at time s (we actually
        # start on time t and don't let it go further than tau - s).
        # So, instead of moving s closer to tau, we move tau closer to s
        tau_cur <- tau - ((s - 1) + current_rvs$s_offset)

        # indicator function. If the current node is a treatment node, it should be zero in the influence curve.
        if(private$is_current_node_treatment(s, intervention, rv = current_rvs$rv)) return(0)

        # If tau ends up to be 0, and we already know y(tau), we can just get
        # it from the data. Expectation of a constant is the constant itself,
        # right?
        if(tau_cur == 0 && current_rvs$rv$getY == var_of_interest) {
          denorm_dat <- self$get_pre_processor$denormalize(dat)
          o_sample_conditional <- as.numeric(denorm_dat[,var_of_interest, with=FALSE])
        } else {
          o_sample_conditional <- private$sample_for_expectation(dat = dat,
                                                                rv = current_rvs$next_rv,
                                                                intervention = intervention,
                                                                tau = tau_cur,
                                                                var_of_interest = var_of_interest)
        }

        tau_cur <- tau - (s - 1)
        o_sample_marginal <- private$sample_for_expectation(dat = dat,
                                                    rv = current_rvs$rv,
                                                    intervention = intervention,
                                                    tau = tau_cur,
                                                    var_of_interest = var_of_interest)

        h_ratio * (o_sample_conditional - o_sample_marginal)
      },

      sample_for_expectation = function(dat, rv, intervention, tau, var_of_interest) {
          o_sample_marginal <- foreach(b = seq(self$get_B), .combine = 'sum') %do% {
            current <- self$get_osl$sample_iteratively(data = dat,
                                              start = rv,
                                              randomVariables = self$get_randomVariables,
                                              intervention = intervention,
                                              tau = tau,
                                              discrete = self$get_discrete,
                                              return_type = 'observations')

            as.numeric(current[tau, var_of_interest, with=FALSE]) / self$get_B 
          } 
      }
    )
)
