% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz.R
\name{get_file_location}
\alias{get_file_location}
\title{Returns the file location for both windows and linux}
\usage{
get_file_location(name, extension, dir = "tmp", subdir = "",
  add_date_to_dir = FALSE, create_dirs = TRUE)
}
\arguments{
\item{name}{the file name to store}

\item{extension}{the extension of the file}

\item{dir}{(default = 'tmp') the directory to store the file in}

\item{subdir}{(default = '') the directory to store the file in}

\item{add_date_to_dir}{(default = FALSE) add a subdir with a date?}

\item{create_dirs}{(default = TRUE) create the dirs specified?}
}
\value{
the location of the file, with the extension and the subdirs
}
\description{
Returns the file location for both windows and linux
}
