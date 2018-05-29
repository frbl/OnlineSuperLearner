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
      initialize = function(verbose = FALSE) {
        private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)
      },

      evaluate = function(osl, simulator, T_iter, B_iter, nbins = (5)) {
        T_iter <- Arguments$getInteger(T_iter, c(1, Inf))
        B_iter <- Arguments$getInteger(B_iter, c(1, Inf))

        pdf('/tmp/density_evaluation.pdf')
        ## For all t in T_iter,
        ## And for all gamma in Gamma,
        sampled_p_values <- foreach(t = 1:T_iter) %do% {

          ## Sample from the simulator B_iter times, independently!
          observed_data = simulator$simulateWAY(numberOfTrajectories = B_iter)
          min_w <- min(observed_data$W)
          max_w <- max(observed_data$W)

          ## Split W in a number of bins. We use the minimal W value to the max W value
          ## and create the nbins over this interval.
          W <- seq(from = min_w, to = max_w, by = abs(max_w-min_w)/nbins)
          W_bins <- data.frame(from = W %>% head(., -1), to = W %>% tail(., -1)) %>% as.matrix %>% t
          rownames(W_bins) <- c('from', 'to')

          ## We assume a discrete or binary treatment variable. Hence, just select
          ## its unique values.
          A_bins <- observed_data$A %>% unique
          n_A_bins <- length(A_bins)

          ## Sample from osl B_iter times
          private$verbose && enter(private$verbose, 'New iteration')
          estimated_data <- foreach(w_bin = W_bins) %do% {
            private$verbose && enter(private$verbose, 'New W bin')

            from <- w_bin['from',1] %>% unname
            to <- w_bin['to',1] %>% unname

            ## TODO: Which W do we actually want to use to sample with?
            ## Currently we use the center of the bin, but I'm not sure
            ## if this is the best option. Another option would be to 
            ## sample a random W on a runif between from and to.
            w <- (from + to)/2

            ## Select the data that falls in this bin
            w_subset <- observed_data[W >= from & W < to,]

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
              newdata <- data.table(W = w, A = a, Y=0)
              nsamples <- (B_iter/nbins)/n_A_bins
              res <- sampledata(osl, newdata, nobs = nsamples)$osl.estimator %>% unlist

              #browser()
              ## Plot some debugging distributions
              plot(density(a_subset$Y))
              lines(density(res), col='red')
              private$verbose && exit(private$verbose)

              ## Calculate kolmogorov smirnov test here.
              pval <- private$test_difference(res, a_subset$Y)

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
        plot(density(sampled_p_values %>% unlist %>% unname))
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
      verbose = NULL,

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
