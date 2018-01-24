#' SMGFactory
#'
#' The \code{SMGFactory} applies the Factory pattern for creating a set of
#' SummaryMeasureGenerators, based on the needed variables. This eases the
#' creation of summary measures as the user of this package only needs to
#' specify the correct column names. The factory parses the formulas used and
#' generates summary measures accordingly.
#'
#' TODO: In its current form the SMG Factory is a rather static class. It is
#' currently hard to extend this function with more SMGS without changing the
#' actual package code. This should be improved.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include SMG.Latest.Entry.R
#' @include SMG.Lag.R
#' @include SMG.Mean.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(candidate_smgs) }}{
#'     Creates a new SMG factory
#'
#'     @param candidate_smgs list UNUSED. This argument should be used to
#'      specify a list of possible SMGs.
#'   }
#'
#'   \item{\code{fabricate(randomVariables, ...) }}{
#'     Fabricates the actual SMG's. Given a list of \code{RandomVariable}s all
#'     covariates are selected and merged.  These are used as a basis for
#'     selecting the needed variables in the dataframe. The column names need to be specified as follows:
#'     \begin{itemize}
#'       \item{just the variable name}: uses the \code{SMG.Latest.Entry} class
#'        to include the contemporaneous variables.
#'       \item{the variable name\_lag\_the lag}: uses the \code{SMG.Lag} class
#'        to include the lagged variables.
#'       \item{the variable name\_mean}: uses the \code{SMG.Mean} class to
#'        include the running mean for this variable.
#'     \end{itemize}
#'
#'     @param randomVariables = a list of \code{RandomVariable} objects, from
#'      which the X variables are selected.
#'
#'     @param ... = data to get passed to the SMG
#'   }
#'
#'   \item{\code{fabricate(randomVariables, ...) }}{
#'     Fabricates the actual SMG's. Given a list of \code{RandomVariable}s all
#'     covariates are selected and merged.  These are used as a basis for
#'     selecting the needed variables in the dataframe.
#'
#'     @param randomVariables list a list of \code{RandomVariable} objects, from which the X variables are selected.
#'
#'     @param ... the data to get passed to the SMG.
#'
#'     @return SummaryMeasureGenerator with the correct SMGs.
#'   }
#'
#'   \item{\code{get_candidate_smgs}}{
#'     Active method. Returns the candidate \code{SummaryMeasureGenerator}s provided on initialization.
#'
#'     @return list the list of \code{candidate_smgs}.
#'   }
#' }
#' @export
SMGFactory <- R6Class("SMGFactory",
  public =
    list(
        initialize = function(candidate_smgs = NULL) {
          # TODO:
          # 1. Inject a list of possible SMG's here
          private$candidate_smgs <- candidate_smgs
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
          mean_params <- private$get_smg_mean_params(needed_variables)
          if (is.a(mean_params, 'list')) {
            SMG.list <- c(SMG.list, SMG.Mean$new(colnames.to.mean = mean_params$colnames.to.mean))
          }

          # Contemporaneos variables
          if (variables_found) {
            needed_variables %<>% setdiff(. ,smg_lag_params$covered_variables)
          }
          SMG.list <- c(SMG.list, SMG.Latest.Entry$new(colnames.to.use = needed_variables))
          SummaryMeasureGenerator$new(SMG.list = SMG.list, ...)
        }
        ),
  active =
    list(
      get_candidate_smgs = function() {
        private$candidate_smgs
      }
    ),
  private =
    list(
      candidate_smgs = NULL,
      get_smg_mean_params = function(needed_variables) {
        my_variables <-  grep('_mean', needed_variables) %>%
            needed_variables[.]

        if (length(my_variables) == 0) { return(FALSE) }

        variables <- my_variables %>%
          gsub("_mean", "",  .) %>%
          unique

        list(colnames.to.mean = variables)
      },
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
