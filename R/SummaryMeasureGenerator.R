#' SummaryMeasureGenerator
#' @importFrom R6 R6Class
#' @importFrom zoo lag
SummaryMeasureGenerator <-
  R6Class (
           "SummaryMeasureGenerator",
           private =
            list(
                ),
           public =
             list(
                  initialize = function(data) {
                  },
                  generate = function(lags){
                    a <- c(1,2,3,4,5,6,7,8)
                    df <- zoo(data.frame(a))
                    b <- lag(df$a,k=c(-1,-2), na.pad= TRUE)
                    m <- mean(df$a)
                    v <- var(df$a)
                    # Moving window mean
                    # Moving window variance / MSSD
                    cbind(df, b, m, v)
                  }
                  )
           )
