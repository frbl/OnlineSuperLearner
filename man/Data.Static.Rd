% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data.Static.R
\docType{class}
\name{Data.Static}
\alias{Data.Static}
\title{Data.Static}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
Data.Static
}
\description{
Class to provide the functionality of treating a static, finite datatable as
a stream of incomming data.
}
\section{Methods}{

\describe{
  \item{\code{initialize(dataset = NULL, url = NULL, verbose = FALSE)}}{
    This method is used to create object of this class. One can provide
    either a \code{datatable} or a \code{url} pointing to a data.frame /
    csvfile (the class uses \code{read.csv}, and it should be compatible with the
    default arguments of that function). Note that the latter is only used
    if the provided \code{datatable} is \code{NULL}. The function will raise
    an error if both dataset and url are \code{NULL}.
    @param dataset (default = NULL) is a datatable to read the data from
    @param url (default = NULL) is a datatable to read the data from if no
     dataset is available
    @param verbose (default = FALSE) the verbosity to be used for logging
  }

  \item{\code{getNext()}}{
    Method to retrieve the next observation from the data frame.
    @return data.table a row from a datatable
  }

  \item{\code{getNextN(n = 1)}}{
    Method that returns the next \code{n} observations. This function can be
    used to bootstrap an initial model or to get a minibatch of
    observations. Note that the function will always try to return data. If
    one asks for n observations, it will check if there are still n new
    observations. If not, it will return all observations still available.
    If there are no observations available, it will return null.
    @param n (default = 1) the number of measurements requested
    @return datatable a row or number of rows from a datatable, NULL if \code{n} <= 0
  }

  \item{\code{reset}}{
    Active function. Method to reset the pointer to the beginning of the datatable.
  }

  \item{\code{get_all}}{
    Active function. Method to retrieve the whole dataset at once.
    @return data.table the \code{data.table} as set when initializing the object
  }

  \item{\code{get_length}}{
    Active function. Method to retrieve the number of rows in the dataframe
    @return integer the number of rows in the dataframe. 0 and a warning if no data is set.
  }

  \item{\code{get_remaining_length}}{
    Active function. Method to retrieve the number of rows still remaining
    in the dataframe
    @return integer the remaining number of rows in the dataframe.
  }

  \item{\code{get_currentrow}}{
    Active function. Method to retrieve pointer to the current row in the
    data frame.
    @return integer the current row in the dataframe. 1 if no data is set.
  }
}
}

\keyword{datasets}
