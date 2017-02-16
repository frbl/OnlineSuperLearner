#' Very basic simulator to generate data from a simple binary model
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom stats glm
#' @field model the most recent / best model fitted.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{Creates a new simulator}
#'
#'   \item{\code{fit(nr.of.observations)}}{Generate a number of observations from the model (multple observations can be retrieved at once by using the \code{nr.of.observations} param}
#'}
Simulator.Simple <-
  R6Class (
           "Simulator.Simple",
           private =
             list(
                  model = NULL
                  ),
           public =
             list(
                  initialize = function() {
                    nobs <- 1000
                    # We have x1 and x2 to create a variable y
                    # some continuous variables
                    x1 <- rnorm(nobs)
                    x2 <- rnorm(nobs, mean=10)

                    # linear combination with a bias
                    z <- 1 + 2*x1 + 1.2*x2

                    # Pass through the inverse logit function
                    pr <- plogis(z)

                    # Generate binary outcomes using this
                    y <- rbinom(nobs, 1, pr)

                    noise <- rnorm(nobs, mean = 0, sd = 1)
                    df <- data.frame(y = y, x1 = x1, x2 = x2, noise = noise)


                    # Store the model, so we can simulate from it later
                    private$model <- glm( y ~ x1 + x2 + noise, data = df, family = "binomial")
                  },

                  getObservation = function(nr.of.observations = 1) {
                    x1 <- rnorm(nr.of.observations)
                    x2 <- rnorm(nr.of.observations)
                    noise <- rnorm(nr.of.observations, mean = 0, sd = 1)
                    df <- data.table(x1 = x1, x2 = x2, noise=noise)
                    y <- plogis(predict(private$model, df))
                    cbind(df, y)
                  }
                  )
           )
