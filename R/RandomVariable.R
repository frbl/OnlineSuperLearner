#' RandomVariable
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
RandomVariable <- R6Class("RandomVariable",
  private =
  list(
        formula = NULL,
        formula.X = NULL,
        formula.Y = NULL,
        family = NULL
      ),
  active =
    list(
        getFamily = function() {
          return(private$family)
        },
        getX = function() {
          return(private$formula.X)
        },
        getY = function() {
          return(private$formula.Y)
        },
        getValidity = function() {
          labels <- unique(attr(terms(formula), 'term.labels'))
          needed <- c(self$getX, self$getY)
          interactionTerms <- setdiff(labels, needed)
          if(length(interactionTerms) != 0) warning(paste('Interactions are not yet supported and are ignored',interactionTerms))
        }
        ),
  public =
    list(
        initialize = function(formula, family) {
          formula.parsed  <- self$parseFormula(formula)

          private$formula = formula
          private$formula.X = formula.parsed$X
          private$formula.Y = formula.parsed$Y
          private$family = family
        },

        parseFormula = function(formula){
          vars <- all.vars(formula)
          depvar <- head(vars, 1)
          names(depvar) <- depvar

          indepvar <- tail(vars, -1)
          names(indepvar) <- indepvar

          list(Y = depvar, X = indepvar)
        }

    )
)
# Static functions
# ================

# Algorithm to find a possible ordering of the functions.
# The worst case run time of this algorithm is pretty bad, and can it
# probably done more efficiently
RandomVariable.find_ordering = function(randomVariables, verbose=FALSE) {
  dependencies <- list()
  order <- c()
  managed_deps <- c()

  for (rv in randomVariables) {
    dependencies[[rv$getY]] <- list(deps = rv$getX, rv= rv)
  }

  for (Y in names(dependencies)) {
    dependencies[[Y]]$deps <- intersect(dependencies[[Y]][['deps']], names(dependencies)) 
  }

  i <- 1
  while(length(dependencies) > 0) {
    if(i > length(dependencies)) {
      throw('Intractable problem! There are interdependencies that cannot be solved!')
    }

    delta <- setdiff(dependencies[[i]]$deps, managed_deps)
    if (length(delta) == 0) {
      current <- names(dependencies)[[i]]
      verbose && cat(verbose, 'Adding', current,'\n')
      order <- c(order, dependencies[[i]]$rv) 
      managed_deps <- c(managed_deps, current) 
      dependencies[[i]] <- NULL
      i <- 1
      next
    }
    i <- i + 1
  }
  names(order) <- managed_deps
  order
}
