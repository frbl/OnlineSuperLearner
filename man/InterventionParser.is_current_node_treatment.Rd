% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InterventionParser.R
\name{InterventionParser.is_current_node_treatment}
\alias{InterventionParser.is_current_node_treatment}
\title{InterventionParser.is_current_node_treatment}
\usage{
InterventionParser.is_current_node_treatment(current_time, intervention,
  current_rv_output)
}
\arguments{
\item{current_time}{the time where we are currently}

\item{intervention}{the specified intervention}

\item{current_rv_output}{the current relevantvariable output}
}
\value{
boolean, whether this is a treatment node
}
\description{
Returns whether the current nodie is a treatment node.
}
