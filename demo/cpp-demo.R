#' @importFrom condensier condensier_options
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach


## Following the guide of the SL3 package
set.seed(49753)
library(sl3)
data(cpp)

cpp <- cpp %>%
  dplyr::filter(!is.na(haz)) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

## covariates = covars, outcome = "haz")



a <- seq(20)
mean_a <- cumsum(a) / seq(length(a))

b <- seq(10)
cums_b <- cumsum(b)

mean_b <- cums_b / seq(length(b))
mean_now <- tail(cums_b,1)
nobs <- length(b)

c <- seq(11, 20)
cumsum(c) + mean_now

mean_c <- (cumsum(c) + mean_now) / seq(nobs+ 1, nobs + length(c))
mean_a - c(mean_b, mean_c)
# -------------------------------------

a <- seq(20)
mean_a <- cumsum(a) / seq(length(a))


b <- seq(10)
cums_b <- cumsum(b)

mean_b <- cums_b / seq(length(b))
mean_now <- tail(mean_b,1)
nobs <- length(b)

c <- seq(11, 20)
cumsum(c) + mean_now

mean_c <- (cumsum(c) + mean_now * nobs) / seq(nobs + 1, nobs + length(c))
mean_a - c(mean_b, mean_c)

