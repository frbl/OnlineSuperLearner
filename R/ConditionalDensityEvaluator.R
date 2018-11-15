#' ConditionalDensityEvaluator
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize() }}{
#'     <<description>>
#'   }
#'   \item{\code{evaluate(osl, T_iter, B_iter) }}{
#'     Set \code{T_iter} to be a large integer (>= 1), an example value for
#'     this would be 100. This is the number of conditional densities we
#'     evaluate. Then set \code{B_iter} to an even larger number (>= 1), e.g.
#'     10e4. This is the number of samples drawn from our distributions.
#'   }
#' }
#'
#' @docType class
#' @importFrom R6 R6Class
ConditionalDensityEvaluator <- R6Class("ConditionalDensityEvaluator",
  public =
    list(
      initialize = function(verbose = FALSE, osl, summary_measure_generator) {
        private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)
        # We dont type check here to support mocks
        private$osl <- osl #Arguments$getInstanceOf(osl, 'OnlineSuperLearner')
        private$summary_measure_generator <- summary_measure_generator #Arguments$getInstanceOf(summary_measure_generator, 'SummaryMeasureGenerator')
        private$evaluation_path <- get_file_location(name = 'density_evaluation', 
                                                     extension = 'pdf')
      },

      evaluate = function(simulator, T_iter, B_iter, nbins = (5)) {
        T_iter <- Arguments$getInteger(T_iter, c(1, Inf))
        B_iter <- Arguments$getInteger(B_iter, c(1, Inf))

        pdf(private$evaluation_path)
        ## For all t in T_iter,
        ## And for all gamma in Gamma,
        sampled_p_values <- foreach(t = 1:T_iter) %do% {

          ## Sample from the simulator B_iter times, independently!
          observed_data = simulator$simulateWAY(numberOfTrajectories = B_iter)

          ## Split W in a number of bins. We use the minimal W value to the max W value
          ## and create the nbins over this interval.

          # 1. Split the dataframe into seperate sections.
          # 2. Make the slicing into each of the different lagged / other vars.
          data <- private$convert_observations(observed_data = observed_data)
          cluster_bins <- private$create_cluster_bins(data, nbins = nbins)

          ## We assume a discrete or binary treatment variable. Hence, just select
          ## its unique values.
          W_bins <- cluster_bins$W
          A_bins <- observed_data$A %>% unique
          n_A_bins <- length(A_bins)

          #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          # WE MOETEN IPV EEN WAARDEN VERZINNEN, EEN HELE RIJ
          # SAMPLEN. DAN IS HET PROBLEEM MET DE LAGS OPGELOST!
          ##################################################

          ## Sample from osl B_iter times
          private$verbose && enter(private$verbose, 'New iteration')
          estimated_data <- foreach(w_bin = W_bins) %do% {
            private$verbose && enter(private$verbose, 'New W bin')

            from <- w_bin['from',1] %>% unname
            to <- w_bin['to',1] %>% unname

            ## Select the data that falls in this bin
            w_subset <- data[W >= from & W < to,]

            res_under_a <- foreach(a = A_bins) %do% {
                
              private$verbose && enter(private$verbose, 'New A bin')

              ## Create a new subset from the W subset in which A = a
              a_subset <- w_subset[A == a,]
              if(nrow(a_subset) < 2) return(NA)

              ## Now we sample a number of observations from our superlearner.
              ## For this we use the number of B_iter terms, and them divide 
              ## by the number of bins (i.e., we assume that each bin has the
              ## same number of observations in it. We do the same for the A bins.
              ## I'm not sure if this is a correct step. The only thing is that I
              ## would like to reduce the number of samples we have to draw from
              ## the osl, and taking a subset felt like a fair way to do this.
              available_subset_size <- nrow(a_subset)
              if (available_subset_size < 1) {
                private$verbose && cat(private$verbose, 'Positivity failed, skipping entry')
                return(NULL)
              }

              newdata <- a_subset[sample(available_subset_size, 1),]
                
              res <- sampledata(private$osl, newdata, 
                                nobs = available_subset_size,
                                summarize = FALSE)$osl.estimator
                
              names <- colnames(res)

              ## Plot some debugging distributions
              predicted_densities <- apply(res, 2, density)
              true_densities <- lapply(names, function(name) density(a_subset[[name]]))
              names(true_densities) <- names

              ##################
              ## Only go for Y now
              plot(true_densities$Y)
              lines(predicted_densities$Y, col='red')
              private$verbose && exit(private$verbose)

              ## Calculate kolmogorov smirnov test here.
              pval <- private$test_difference(res$Y, a_subset$Y)
              #if(pval <= 0.05) browser()
              pval
            }


            names(res_under_a) <- A_bins
            private$verbose && exit(private$verbose)
            res_under_a
          }
          private$verbose && exit(private$verbose)

          names(estimated_data) <- apply(W_bins, 2, function(x) paste(x, collapse=' to '))

          estimated_data
        }

        ## TODO: For some reason the difference testing can result in NA. Look into this.
        density_vals <- sampled_p_values %>% unlist %>% unname
        plot(density( density_vals[!is.na(density_vals)]))
        dev.off()

        ## TODO: We still need to perform the bonferoni correction.
        return(sampled_p_values)
      }
    ),
  active =
    list(
    ),
  private =
    list(
      evaluation_path = NULL,
      verbose = NULL,
      osl = NULL,
      summary_measure_generator = NULL,

      convert_observations = function(observed_data) {
        data <- Data.Static$new(dataset = observed_data)
        private$summary_measure_generator$set_trajectories(data = data)
        needed_for_history <- private$summary_measure_generator$get_minimal_measurements_needed
        data <- private$summary_measure_generator$getNext(n = nrow(observed_data) - needed_for_history)

        ## Only use the first trajectory
        data[[1]]
      },

      create_cluster_bins = function(data, nbins) {
        min_values <- lapply(data, min)
        max_values <- lapply(data, max)

        bins <- apply(cbind(min_values, max_values), 1, function(entry) { 
                min <- entry$min_values
                max <- entry$max_values
                seq(from = min, to = max, by = abs(max - min) / nbins)
        })

        names <- colnames(bins)
        cluster_bins <- lapply(names, function(colname) {
          data.frame(from = bins[,colname] %>% 
                              head(., -1), to = bins[,colname] %>% 
                              tail(., -1)) %>% as.matrix %>% t
        })
        names(cluster_bins ) <- names
        cluster_bins
      },

      test_difference = function(x, y, ...) {
        ## Perform the kolmogorov smirnov test here. If the resulting p value is
        ## larger than 0.05, we assume that there is no significant difference 
        ## between the distributions. If the p is indeed < 0.05, there is a significant
        ## difference between the two distributions.
        test_res <- ks.test(x, y, "two.sided")
        test_res$p
      }
    )
)
