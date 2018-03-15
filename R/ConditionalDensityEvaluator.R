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
      initialize = function() {
      },

      evaluate = function(osl, simulator, T_iter, B_iter, nbins = (4*5)) {
        T_iter <- Arguments$getInteger(T_iter, c(1, Inf))
        B_iter <- Arguments$getInteger(B_iter, c(1, Inf))
        ## For all t in T_iter,
        ## And for all gamma in Gamma,

        ## Sample from the simulator B_iter times
        observed_data = simulator$simulateWAY(numberOfBlocks = B_iter)
        min_w <- min(observed_data$W)
        max_w <- max(observed_data$W)

        W <- seq(from = min_w, to = max_w, by = abs(max_w+min_w)/nbins)
        
        # TODO:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # W should not be between -ing and inf, but wmin and wmax
        W_bins <- data.frame(from = c(-Inf, W), to = c(W,Inf)) %>% as.matrix %>% t
        rownames(W_bins) <- c('from', 'to')

        A <- c(0,1)
        ## Sample from osl B_iter times
        estimated_data <- foreach(w = W_bins) %do% {
          from <- w['from',1] %>% unname
          to <- w['to',1] %>% unname
          subset <- observed_data[W > from & W < to]
          foreach(a = A ) %do% {
            subset <- subset[A == a]
            newdata <- data.table(W = from, A = a, Y=0)
            res <- foreach(seq(1,(B_iter/nbins)/2)) %dopar% {
              sampledata(osl, newdata)$osl.estimator
            } %>% unlist
            ## Calculate kolmogorov smirnov test here.
            private$test_difference(res, subset$Y)
          }
        }

        estimated_data[[3]] %>% length
        browser()
      }
    ),
  active =
    list(
    ),
  private =
    list(

      test_difference = function(x, y, ...) {
        browser()
        test_res <- ks.test(x, y, "two.sided")
        test_res
      }
    )
)
