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

        pdf('/tmp/hoi.pdf')
        ## For all t in T_iter,
        ## And for all gamma in Gamma,
        sampled_p_values <- foreach(t = 1:T_iter) %do% {

          ## Sample from the simulator B_iter times, independently!
          observed_data = simulator$simulateWAY(numberOfTrajectories = B_iter)
          min_w <- min(observed_data$W)
          max_w <- max(observed_data$W)

          W <- seq(from = min_w, to = max_w, by = abs(max_w-min_w)/nbins)
          
          ## Note we don't include min_w, as the seq already includes this min.
          ## The max is appended as this one is not included.
          W_bins <- data.frame(from = W %>% head(., -1), to = W %>% tail(., -1)) %>% as.matrix %>% t
          rownames(W_bins) <- c('from', 'to')

          ## We assume a discrete or binary treatment variable 
          A_bins <- observed_data$A %>% unique

          ## Sample from osl B_iter times
          private$verbose && enter(private$verbose, 'New iteration')
          estimated_data <- foreach(w_bin = W_bins) %do% {
            private$verbose && enter(private$verbose, 'New W bin')

            from <- w_bin['from',1] %>% unname
            to <- w_bin['to',1] %>% unname
            w <- (from + to)/2

            w_subset <- observed_data[W >= from & W < to]

            res_under_a <- foreach(a = A_bins) %do% {
              private$verbose && enter(private$verbose, 'New A bin')
              subset <- w_subset[A == a]
              if(nrow(subset) < 2) return(NA)

              res <- foreach(seq(1,(B_iter/nbins)/2)) %dopar% {
                w = sample(w_subset$W, 1)
                newdata <- data.table(W = w, A = a, Y=0)
                sampledata(osl, newdata)$osl.estimator
              } %>% unlist

              ## Plot some debugging distributions
              #if(nrow(subset) > 60) browser()
              plot(density(subset$Y))
              lines(density(res), col='red')
              private$verbose && exit(private$verbose)

              ## Calculate kolmogorov smirnov test here.
              pval <- private$test_difference(res, subset$Y)
              if(pval <= 0.05) browser()
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
        sampled_p_values
      }
    ),
  active =
    list(
    ),
  private =
    list(
      verbose = NULL,

      test_difference = function(x, y, ...) {
        test_res <- ks.test(x, y, "two.sided")
        test_res$p
      }
    )
)
