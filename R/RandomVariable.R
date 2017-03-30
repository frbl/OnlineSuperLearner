#' RandomVariable
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
RandomVariable <-
  R6Class (
           "RandomVariable",
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
