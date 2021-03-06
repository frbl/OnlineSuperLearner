% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WCC.CG.R
\docType{class}
\name{WCC.CG}
\alias{WCC.CG}
\title{WCC.CG
Constrained descent optimizer}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
WCC.CG
}
\description{
WCC.CG
Constrained descent optimizer
}
\section{Methods}{

\describe{
  \item{\code{initialize(weights.initial)}}{
   Creates a new computer for determining the best weighted combination of
   the ML libraries based on stochastic gradient descent and simplex
   projection.
   @param weights.initial vector (default = NULL) the initial vector of
    weights to use. Can be NULL if the number_of_algorithms is specified.

   @param number_of_algorithms integer (default = NULL) the number of
   algorithms to calculate the weights for. Is used to initialize the
   weights. Can be NULL if the weights are provided
  }

  \item{\code{compute(Z, Y, libraryNames, ...)}}{
   Method to compute the best weighted combination for a set of estimators.
   In this implementation we use a two-step approach, in which we first
   estimate the weights based on a gradient descent procedure, and then
   project these weights back to the L1 simplex, scaling them between 0-1.

   @param Z matrix containing the outcomes of each of the estimators

   @param Y vector containing the actual observed outcome.

   @param libraryNames vector containing the names of the optimizer.

   @param ... other parameters to pass to the underlying combination
    computers.

   @return vector of the trained / updated weights.
  }
}
}

\keyword{datasets}
