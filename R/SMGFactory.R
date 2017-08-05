#' SMGFactory
#'
#' Factory for creating a set of SummaryMeasureGenerators, based on the needed variables
#' @docType class
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize() }}{
#'     Creates a new SMG factory
#'   }
#'
#'   \item{\code{fabricate(randomVariables, verbose = FALSE) }}{
#'     Fabricates the actual SMG's. Given a list of \code{RandomVariable}s all covariates are selected and merged.
#'     These are used as a basis for selecting the needed variables in the dataframe.
#'     @param randomVariables = a list of \code{RandomVariable} objects, from which the X variables are selected
#'     @param ... = data to get passed to the SMG
#'   }
#' }
#' @export
SMGFactory <- R6Class("SMGFactory",
  public =
    list(
        initialize = function(candidate_smgs = NULL) {
          # TODO:
          # 1. Inject a list of possible SMG's here
          # 2. let each define a function that describes the variables they need, given the randomvariables
          # 3. Make a loop in here that processes the RV's
          # This allows for flexible, new SMG's to be added outside of the package.
        },

        fabricate = function(randomVariables, ...) {
          SMG.list <- list()
          variables_found <- FALSE

          needed_variables <- lapply(randomVariables, function(rv) c(rv$getX, rv$getY)) %>%
            unlist %>%
            unique

          # Process lags
          smg_lag_params <- private$get_smg_lag_params(needed_variables)
          if (is.a(smg_lag_params, 'list')) {
            variables_found <- TRUE
            SMG.list <- c(SMG.list, SMG.Lag$new(lags = smg_lag_params$lags,
                                                colnames.to.lag = smg_lag_params$colnames.to.lag))
          }
          # Process other stuff

          # Contemporaneos variables
          if (variables_found) {
            needed_variables %<>% setdiff(. ,smg_lag_params$covered_variables)
          }
          c(SMG.list, SMG.Latest.Entry$new(colnames.to.use = needed_variables)) %>%
            SummaryMeasureGenerator$new(SMG.list = ., ...) 
        }
        ),
  active =
    list(
        ),
  private =
    list(
      get_smg_lag_params = function(needed_variables) {
        my_variables <-  grep('_lag_', needed_variables) %>%
            needed_variables[.] 
          
        if (length(my_variables) == 0) { return(FALSE) }

        lags <- my_variables %>%
          gsub("^.+?(_lag_)", "",  .) %>%
          strtoi %>%
          max

        variables <- my_variables %>%
          gsub("_lag_[0-9]*", "",  .) %>%
          unique

        list(lags = lags, colnames.to.lag = variables, covered_variables = my_variables)
      }
    )
)
