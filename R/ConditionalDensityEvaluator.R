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
#' @importFrom ggplot2 ggplot geom_density
#' @importFrom reshape2 melt
#' @importFrom stats ecdf
ConditionalDensityEvaluator <- R6Class("ConditionalDensityEvaluator",
  public =
    list(
      initialize = function(verbose = FALSE, osl, summary_measure_generator, cfg = 1) {
        private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)
        # We dont type check here to support mocks
        private$osl <- osl #Arguments$getInstanceOf(osl, 'OnlineSuperLearner')
        private$summary_measure_generator <- summary_measure_generator #Arguments$getInstanceOf(summary_measure_generator, 'SummaryMeasureGenerator')
        private$evaluation_path <- get_file_location(name = paste('density_evaluation', cfg, sep='_'), 
                                                     extension = 'pdf')
      },

      evaluate = function(simulator, T_iter, B_iter, nbins = (5), outcome_variable = NULL) {
        T_iter <- Arguments$getInteger(T_iter, c(1, Inf))
        B_iter <- Arguments$getInteger(B_iter, c(1, Inf))

        pdf(private$evaluation_path)
        ## For all t in T_iter,
        ## And for all gamma in Gamma,
        sampled_p_values <- foreach(t = 1:T_iter) %do% {

          ## Sample from the simulator B_iter times, independently!
          ## Note that this is raw data (not normalized!)
          #observed_data = simulator$simulateWAY(numberOfTrajectories = B_iter)
          observed_data = simulator$simulateWAY(numberOfBlocks = B_iter)


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
                
              ## Sample a single row from the whole set of basedata
              res <- foreach(seq(1,available_subset_size)) %dopar% {
                idx <- sample(available_subset_size, 1)
                sub_res <- sampledata(private$osl, a_subset[idx,], 
                                  nobs = 1,
                                  Y = outcome_variable,
                                  summarize = FALSE)
                return(list(osl = sub_res$osl.estimator, dosl = sub_res$dosl.estimator))
              }
              res <- lapply(res, as.data.frame) %>% rbindlist 
              colnames(res) <- c('osl.estimator', 'dosl.estimator')

              ## Plot some debugging distributions
              ## Only go for Y now
              ## First make sure to convert the observed data back to its original form
              current_Y <- private$convert_normalized(a_subset)$Y

              private$plot_densities(current_Y, res$osl.estimator, res$dosl.estimator, A_bin = a)


              ## Calculate kolmogorov smirnov test here.
              pval <- private$test_difference(res$dosl.estimator, current_Y)
              private$verbose && exit(private$verbose)

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
        private$plot_pvalue_density(sampled_p_values)

        dev.off()

        return(sampled_p_values)
      },

      calculate_significance = function(result, bonferroni_correction = TRUE, alpha = 0.05) {
        flat_result <- result %>% unlist %>% unname
        flat_result <- flat_result[!is.na(flat_result)]

        if (bonferroni_correction) {
          alpha <- alpha/(T_iter * nbins)
        }

        sum(flat_result < alpha) / length(flat_result) * 100 %>% round(., 2)
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

      plot_pvalue_density = function(pvalues) {
        density_vals <- pvalues %>% 
          unlist %>% 
          unname %>% 
          data.frame(density_vals = .)

        density_plot <- ggplot(density_vals, aes(density_vals)) + 
          geom_density() 
        plot(density_plot)
        plot(ecdf(density_vals$density_vals))
      },

      plot_densities = function(observed_data, osl, dosl, A_bin) {
        melted_df <- data.frame(observed = observed_data, osl = osl, dosl = dosl) %>%
          melt(., measure.vars = colnames(.))

        density_plot <- ggplot(melted_df, aes(x=value, fill=variable)) + 
          geom_density(alpha=0.25) +
          guides(fill=guide_legend(title=paste("Number of observations:", length(observed_data), 'A:', A_bin)))

        density_plot %T>% plot
      },

      convert_normalized = function(normalized_data) {
        private$summary_measure_generator$get_pre_processor$denormalize(normalized_data)
      },

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
