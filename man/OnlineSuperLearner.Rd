% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OnlineSuperLearner.R
\docType{class}
\name{OnlineSuperLearner}
\alias{OnlineSuperLearner}
\title{OnlineSuperLearner}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
OnlineSuperLearner
}
\description{
This is the main super learner class. This class contains everything related
to the super learner machine learning model.
}
\section{Methods}{

\describe{
  \item{\code{new(SL.Library)}}{starts a new OnlineSuperLearner. The provided /code{SL.Library} contains the machine learning models to use}

  \item{\code{run(data, X, Y, initial.data.size = 10)}}{runs the actual OnlineSuperLearning calculation}
  \item{\code{getModel()}}{returns the final OnlineSuperLearner model}
  \item{\code{predict(data, X)}}{returns an actual prediction using the superlearning model}
}
}
\keyword{datasets}
