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
#'   \item{\code{evaluation_of_conditional_expectations(data, h_ratio_predictors}}{ 
#'     In this function one can perform the second step of OOS, calculate the conditional expectations / difference in
#'     conditional expectations. 
#'     @param data the data to seed the sampling procedure and thereby conditional expectation evaluation. 
#'     @param h_ratio_predictors the list of predictors, in a format returned using the \code{get_h_ratio_estimators}
#'     format.
#'   } 
#' 
#' }  
#' @docType class
#' @include ConstrainedGlm.R
#' @include DataCache.R
#' @import methods
#' @import R.oo
#' @import R.utils
#' @importFrom R6 R6Class
OneStepEstimator <- R6Class("OneStepEstimator",
  class = FALSE,
  cloneable = FALSE,
  portable = FALSE,
  public =
    list(
      initialize = function(osl, randomVariables, N, B, pre_processor, tau, intervention, variable_of_interest, discrete = TRUE, parallel= TRUE, online = FALSE, verbose = FALSE) {
        private$osl <- Arguments$getInstanceOf(osl, 'OnlineSuperLearner')

        private$last_oos_estimate <- 0
        private$N <- Arguments$getInteger(N, c(1, Inf))
        private$B <- B##Arguments$getIngeger(B, c(1, Inf))
        private$discrete <- Arguments$getLogical(discrete)
        private$randomVariables <- Arguments$getInstanceOf(randomVariables, 'list')
        private$pre_processor <- pre_processor
        private$tau <- tau
        private$intervention <- intervention
        private$variable_of_interest <- Arguments$getCharacters(variable_of_interest$getY)
        private$is_parallel <- parallel
        private$online <- online
        private$P_data_cache <- DataCache$new(online = self$is_online)
        private$Pstar_data_cache <- DataCache$new(online = self$is_online)
        private$verbose <- Arguments$getVerbose(-8, timestamp=TRUE)
        #private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)
      },

      perform = function(initial_estimate, data, truth = NULL) {
        private$verbose && enter(private$verbose, 'Starting efficient influence curve estimation')
        #}

        ## Calculate the new estimate
        oos_estimate <- self$calculate_full_oos(initial_estimate = initial_estimate, data = data, truth = truth)

        ## Calculate the new variance
        oos_variance <- self$calculate_oos_variance()

        result <- list(
          oos_estimate = oos_estimate,
          oos_variance = oos_variance
        )

        private$verbose && cat(private$verbose, 'Final result: ', result)
        private$verbose && exit(private$verbose)
        result
      },

      calculate_full_oos = function(initial_estimate, data, truth = NULL) {
        ## Let $B$ be a large integer (the larger $B$ the better the approxmation),
        ## let $N$ be the number of observations,
        N <- nrow(data)

        ## let intervention be the intervention we whish to oppose on the system, and
        ## let tau the the outcome of interest.
        initial_estimate <- Arguments$getNumerics(initial_estimate)

        #D_star_evaluation <- foreach(t = seq(1:N), .combine = 'sum') %do% {
        D_star_evaluation <- self$get_last_oos_estimate

        #for (t in 1:N) {
        t = 1
        current_data <- data#[t,]

        ## 1. calculate H-ratios
        ## Note that the last_h_ratio_estimators parameter is either null (i.e., this is the first run),
        ## Or contains the h_ratios of the previous iteration
        private$last_h_ratio_estimators <- self$get_h_ratio_estimators(
          data = current_data, 
          last_h_ratio_estimators = self$get_last_h_ratio_estimators
        )

        ## 2. Solve expectaions
        ## We want to approximate the conditional expectation from RV (start) till Ytau
        one_iteration_D_star_evaluation <- self$evaluation_of_conditional_expectations(
          data = current_data,
          h_ratio_predictors = self$get_last_h_ratio_estimators
        )

        ## 3. Online update the dstar value
        D_star_evaluation <- (((t-1) * D_star_evaluation) + one_iteration_D_star_evaluation) / t

        private$verbose && cat(private$verbose, 'Current D-star evaluation is ', one_iteration_D_star_evaluation,
                                                ' Total evaluation is ', D_star_evaluation, 
                                                ' and t (iteration) is ', t)

        if(!is.null(truth)) {
          private$verbose && cat(private$verbose, 'Abs difference with truth: ', abs((truth - (initial_estimate + D_star_evaluation))),
                                                  ' Initial difference with truth: ', abs(truth - initial_estimate ))
        }

        # }

        ## Store the last OOS (for when it becomes online)
        private$last_oos_estimate <- D_star_evaluation

        oos_estimate <- D_star_evaluation + initial_estimate
        if(is.nan(oos_estimate)) {
          warning('Oos estimate is NaN, setting to zero')
          oos_estimate <- 0
        }
        oos_estimate
      },

      calculate_oos_variance = function() {
        ## TODO: Implement
        return(0)
      },

      get_h_ratio_estimators = function(data, last_h_ratio_estimators = NULL) {
        private$verbose && enter(private$verbose, 'Getting the h-ratio estimators')

        `%looping_function%` <- private$get_looping_function()

        ## We first sample $B$ observations from $P^N$ (that is, N blocks of
        ## summary relevant history) and then $BN$ observations from $P^N_{s,a}
        ## (that is, $B * N * \tau$ blocks$ of relevant history).
        ## The size of Osample will be $B N$.
        ##
        ## After we have sampled these $BN$ observations, we augment the data
        ## with a $\Delta$ column, which is either $1$ (when the data was
        ## sampled from P^N), or $0$, whenever the data was sampled from
        ## $P^N_{a,s}$. Using these fully augmented data frames we now create
        ## $3$ (one for each W,A,Y) times $\tau$ machine learning estimators.
        private$verbose && enter(private$verbose, 'Starting sampling from PN')
        self$print_parallel
        tic <- Sys.time()

        N <- self$get_N#nrow(data)

        O_0 <- data[1,]
        ## Run B iterations on O_0 (always start from the same data
        Osample_p <- foreach(b = seq(1:self$get_B), .combine = rbind) %looping_function% {

          ## TODO: Note that the summary measures we currently collect are
          ## NORMALIZED. I think that this does not matter for calculating the
          ## h-ratios as the ratio stays the same (it might even work better),
          ## but we need to check this.
          private$verbose && cat(private$verbose, 'PN sample - iteration ', b)
          current <- self$get_osl$sample_iteratively(data = O_0,
                                                     randomVariables = self$get_randomVariables,
                                                     tau = N,
                                                     discrete = self$get_discrete,
                                                     intervention = NULL,
                                                     return_type = 'full')
          current
        }

        toc <- Sys.time()
        private$verbose && cat(private$verbose, toc - tic)
        private$verbose && exit(private$verbose)
        private$verbose && cat(private$verbose, 'Sampled ', self$get_B,' observations from PN.')

        private$verbose && enter(private$verbose, 'Starting sampling from PN*')
        tic <- Sys.time()

        ## Because we use $BN$ observations in the previous sampling step, we
        ## should also draw BN observations from P^N_{s,a}.
        Osample_p_star <- foreach(b = seq(self$get_B * N), .combine = rbind) %looping_function% {
          private$verbose && cat(private$verbose, 'PN* sample - iteration ', b)
          current <- self$get_osl$sample_iteratively(data = O_0,
                                                     randomVariables = self$get_randomVariables,
                                                     tau = self$get_tau,
                                                     discrete = self$get_discrete,
                                                     intervention = self$get_intervention,
                                                     return_type = 'full')
          current
        }

        #browser()
        toc <- Sys.time()
        private$verbose && exit(private$verbose)
        private$verbose && cat(private$verbose, 'Sampled ', self$get_B * N,' observations from PN*')
        private$verbose && cat(private$verbose, toc - tic)

        P_rows <- nrow(Osample_p)
        Pstar_rows <- nrow(Osample_p_star)

        ## Now add the Delta column so we know which blocks belong to PN* and which to PN
        Osample_p[, Delta := rep(1, P_rows)]
        Osample_p_star[, Delta := rep(0, Pstar_rows)]

        ## Add an S column to the data, so we know which summary measure belongs to which s
        time_s_column <- lapply(seq(Pstar_rows), function(i) ((i - 1) %% self$get_tau) + 1)  %>% unlist
        Osample_p_star[, time_s_column := time_s_column]

        rbindlist(list(Osample_p, Osample_p_star[time_s_column == 1][,!'time_s_column']))
        ## Perform the actual learning of the h_ratio predictors
        h_ratio_predictors_per_s <- self$calculate_h_ratio_predictors(Osample_p, Osample_p_star)

        ## The final result is a list of estimators, which contains a GLM for
        ## each $C_W$, $C_A$, and $C_Y$, for each s in tau.  (so 3tau
        ## estimators)
        private$verbose && exit(private$verbose)
        return(h_ratio_predictors_per_s)
      },

      evaluation_of_conditional_expectations = function(data, h_ratio_predictors) {
        private$verbose && enter(private$verbose, 'Evaluating the conditional expectations')
        self$print_parallel

        ## We have to create the conditional expectations using each of the
        ## random variables as a start point
        influence_curve_for_each_rv <- c()
        N <- self$get_N#nrow(data)

        ## This is the outer loop of the influence curve
        #browser()
        efficient_influence_curve <- foreach(t=seq(N), .combine='sum') %do% {
          private$verbose && cat(private$verbose, 'Efficient influence curve iteration ', t)
          current_dat <- data[t,]

          difference_in_expectations_for_all_s <- 
            ## This is the inner loop of the influence curve (for 1 to tau)
            foreach(time_s=seq(self$get_tau), .combine='sum') %:% 
              ## Then, we start on a given s, this is for each D_x in D
              foreach(rv_id=seq_along(self$get_randomVariables), .combine='sum') %do% {
                private$verbose && cat(private$verbose, 'This is RV: ', rv_id, ' for s: ', time_s)
                current_rvs <- self$get_next_and_current_rv(rv_id)
                private$get_influence_curve_for_one_random_variable(
                  s = time_s,
                  dat = current_dat,
                  h_ratio_predictors = h_ratio_predictors,
                  current_rvs = current_rvs
                )
          }

          ## Divide within the loop so we reduce the risk of integer overflows.
          ## Check if this is the case.
          difference_in_expectations_for_all_s / N
        }
        private$verbose && exit(private$verbose)
        efficient_influence_curve
      },

      calculate_h_ratio_predictors = function(Osample_p, Osample_p_star) {
        ## Get the list of formulae for which we need to calculate the h_ratio_predictors
        formulae <- self$get_formulae

        ## Find the time of the first intervention. Before this first
        ## intervention no divergence should be possible.
        first_intervention_time <- InterventionParser.first_intervention(self$get_intervention)

        ########################
        # Fake the Online part #
        ########################
        data_cache <- self$get_data_cache(star = FALSE)
        data_cache$update_cache(newdata = Osample_p)

        star_data_cache <- self$get_data_cache(star = TRUE)
        star_data_cache$update_cache(newdata = Osample_p_star)

        ## We generate tau estimators here, one for each s to tau
        lapply(seq(self$get_tau), function(time_s) {

          ## If no intervention is given, we do not have to calculate the
          ## ratio, as it is always (approximately) 1.
          if(time_s < first_intervention_time) {
            h_ratio_predictors <- rep(NA, length(formulae)) %>% as.list
          } else {
            ## Merge the two data tables, while selecting only the current time s object
            Osample_p_full <- rbindlist(
              list(
                data_cache$get_data_cache,
                ## Remove the time_s_column
                star_data_cache$get_data_cache[time_s_column == time_s][,!'time_s_column']
              )
            )

            private$verbose && cat(private$verbose, 'Calculating H-estimator for time ', time_s)

            h_ratio_predictors <- lapply(formulae, function(formula){
              ## TODO: Currently we use GLM here, but we should make use of a SuperLearner here.
                                           #browser()
              hide_warning_convergence(
                ConstrainedGlm.fit(formula = formula(formula),
                                  data = Osample_p_full,
                                  delta = 0.05,
                                  previous_glm = private$get_previous_h_ratio_estimator(formula = formula, s = time_s))
              )
              #formula
              #cop <- Osample_p_full
              #form <- Delta ~ A_lag_1 + W_lag_1 + Y_lag_1 + Y_lag_2
              #ConstrainedGlm.fit(formula = formula(form),
                                #data = cop,
                                #delta = 0.05,
                                #previous_glm = private$get_previous_h_ratio_estimator(formula = formula, s = time_s))
            })
          }
          ## Store the names of the formulae, so we can index them easily later on
          names(h_ratio_predictors) <- formulae
          h_ratio_predictors
        })

      },

      calculate_h_ratio = function(h_ratio_predictors, s, formula, data) {
        if(is.null(h_ratio_predictors)) {
          ## This is just for testing purposes!
          warning('Test function was called!')
          return(0.5)
        }
        predictor_or_na <- h_ratio_predictors[[s]][[formula]]

        ## If we didn't find a predictor_or_na, it means that the the numerator and
        ## denominator are equal. Hence their ratio = 1. This is the case for
        ## the pre-treatment distributions.
        ## It is null if the matrix was singular.
        if (is.null(predictor_or_na) || (is(predictor_or_na, 'logical') && is.na(predictor_or_na))) {
          return(1) 
        }

        ## Make the actual H prediction
        h_prediction <- ConstrainedGlm.predict(constrained_glm = predictor_or_na, newdata = data)

        h_ratio <- (h_prediction / (1.0 - h_prediction))
        if (h_ratio > 19 || h_ratio < 0.05) {
          warning('Calculated h_ratio is relatively large or very small! Are you violating the positivity assumption? Value=', h_ratio, '. Were setting it to 0, to be sure')
          warning('Data used: ', paste(data, collapse = '-'))
          h_ratio = 0 
        }

        h_ratio
      },

      calculate_difference_in_expectations = function(s, dat, formula, current_rvs) {
        ## Now we have to sample from the conditional distribution of rv + 1.
        ## I.e., we have the our value for RV, and its corresponding C. Using
        ## these values we can sample the next variable in the sequence.

        ## indicator function. If the current node is a treatment node, it should be zero in the influence curve.
        current_node_is_treatment <- InterventionParser.is_current_node_treatment(current_time=s, 
                                                        intervention = self$get_intervention, 
                                                        current_rv_output = current_rvs$rv$getY)
        if(current_node_is_treatment) return(0)

        ## Calculate whether the next variable is still in the same S (the next after Y(s) is W(s +1)
        starting_time_for_next_rv <- Arguments$getNumerics(s + current_rvs$s_offset)

        ## If tau ends up to be 0, and we already know y(tau), we can just get
        ## it from the data. Expectation of a constant is the constant itself,
        ## right?
        if(self$get_tau == s && current_rvs$rv$getY == self$get_variable_of_interest) {
          denorm_dat <- self$get_pre_processor$denormalize(dat)
          o_sample_conditional <- as.numeric(denorm_dat[,self$get_variable_of_interest, with=FALSE])
        } else {
          ## this is the first part of the difference in expectations
          o_sample_conditional <- private$sample_for_expectation(dat = dat,
                                                                 start_from_variable = current_rvs$next_rv,
                                                                 start_from_time = starting_time_for_next_rv)
        }

        ## this is the second part of the difference in expectations
        o_sample_marginal <- private$sample_for_expectation(dat = dat,
                                                            start_from_variable = current_rvs$rv,
                                                            start_from_time = s)

        ## Return the difference of the two expected values
        (o_sample_conditional - o_sample_marginal)
      },

      ## Function is public so it can easiliy be tested
      get_next_and_current_rv = function(current_rv_index) {
        ## Find the ID for the next random variable
        next_rv_id <- (current_rv_index %% length(self$get_randomVariables)) + 1
        rv <- self$get_randomVariables[[current_rv_index]]
        next_rv <- self$get_randomVariables[[next_rv_id]]

        ## We are actually in next time block if the modulo was applied. This should be reflected in the time s.
        s_offset <- ifelse(current_rv_index > next_rv_id, 1, 0)
        list(rv = rv, next_rv = next_rv, s_offset = s_offset)
      },

      get_data_cache = function(star = FALSE) {
        if (star) return(private$Pstar_data_cache)
        return(private$P_data_cache)
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

      get_formulae = function() {
        lapply(self$get_randomVariables, function(rv) {
          formula <- rv$get_formula_string(Y = 'Delta')
        }) %>% unique
      },

      get_discrete = function() {
        return(private$discrete)
      },

      get_randomVariables = function() {
        return(private$randomVariables)
      },

      get_pre_processor = function() {
        return(private$pre_processor)
      },

      get_variable_of_interest = function() {
        private$variable_of_interest
      },

      get_tau = function() {
        private$tau
      },

      get_intervention = function() {
        private$intervention
      },

      get_last_oos_estimate = function() {
        private$last_oos_estimate
      },

      get_last_h_ratio_estimators = function() {
        private$last_h_ratio_estimators
      },

      is_online = function() {
        private$online
      },

      print_parallel = function() {
        if (private$is_parallel) {
          private$verbose && cat(private$verbose, 'In parallel')
        }
      }
    ),
  private =
    list(
      verbose = NULL,
      osl = NULL,
      N = NULL,
      B = NULL,
      online = NULL,
      randomVariables = NULL,
      variable_of_interest = NULL,
      discrete = NULL,
      pre_processor = NULL,
      tau = NULL,
      intervention = NULL,
      last_oos_estimate = NULL,
      last_h_ratio_estimators = NULL,
      is_parallel = NULL,
      P_data_cache = NULL,
      Pstar_data_cache = NULL,

      get_previous_h_ratio_estimator = function(s, formula) {
        estimators <- self$get_last_h_ratio_estimators
        if (is.null(estimators) || !self$is_online) return(NULL)
        estimators[[s]][[formula]]
      },

      get_looping_function = function() {
        if(private$is_parallel) {
          return(`%dopar%`)
        }
        return(`%do%`)
      },

      get_influence_curve_for_one_random_variable = function(s, dat, h_ratio_predictors, current_rvs) {
        ## Get the formula ant output name so we can retrieve the prediction mechanism.
        formula    <- current_rvs$rv$get_formula_string(Y = 'Delta')

        ## 1.  we have to estimate the h_ratio based on the C_rv, and the current s and random variable
        h_ratio    <- self$calculate_h_ratio(h_ratio_predictors, s, formula, dat)

        ## We don't have to go through all the trouble if the h_ratio is 0 (0 * x = 0)
        if(h_ratio == 0) return(0)

        ## 2. Calculate the difference between the expected values
        difference <- self$calculate_difference_in_expectations(s, dat, formula, current_rvs)

        h_ratio * difference
      },


      sample_for_expectation = function(dat, start_from_variable, start_from_time) {
        `%looping_function%` <- private$get_looping_function()
        foreach(b = seq(self$get_B), .combine = 'sum') %looping_function% {
          current <- self$get_osl$sample_iteratively(data = dat,
                                                     start_from_variable = start_from_variable,
                                                     start_from_time = start_from_time,
                                                     randomVariables = self$get_randomVariables,
                                                     intervention = self$get_intervention,
                                                     tau = self$get_tau,
                                                     discrete = self$get_discrete,
                                                     return_type = 'observations')

          ## Calculate the average over all bootstrap iterations
          res <- as.numeric(current[nrow(current), self$get_variable_of_interest, with=FALSE]) 
          res <- res / self$get_B 
          if(is.na(res)) browser()
          res
        } 
      }
    )
)
