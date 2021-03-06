% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Evaluation.R
\name{Evaluation.mean_squared_error}
\alias{Evaluation.mean_squared_error}
\title{Evaluation.mean_squared_error}
\usage{
Evaluation.mean_squared_error(data.observed, data.predicted)
}
\arguments{
\item{data.observed}{the true data (Y)}

\item{data.predicted}{the Y outcome from the estimator}
}
\value{
double the calculated MSE.
}
\description{
Function to calculate the mean squared error (uses the mse loss function internally).
}
