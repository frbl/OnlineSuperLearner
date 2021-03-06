% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Evaluation.R
\name{Evaluation.log_loss}
\alias{Evaluation.log_loss}
\title{Evaluation.log_loss}
\usage{
Evaluation.log_loss(data.observed, data.predicted, eps = 1e-15)
}
\arguments{
\item{data.observed}{the true data (Y).}

\item{data.predicted}{the Y outcome from the estimator.}

\item{eps}{is a small offset to let the log not go to Inf.}
}
\value{
double the calculated log loss.
}
\description{
Function to calculate the log loss evaluation metric.
}
