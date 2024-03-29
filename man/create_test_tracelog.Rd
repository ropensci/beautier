% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_test_tracelog.R
\name{create_test_tracelog}
\alias{create_test_tracelog}
\title{Create a \code{tracelog} object, as used for testing}
\usage{
create_test_tracelog(
  filename = create_temp_tracelog_filename(),
  log_every = 1000,
  mode = "autodetect",
  sanitise_headers = TRUE,
  sort = "smart"
)
}
\arguments{
\item{filename}{name of the file to store the posterior traces.
Use \link{NA} to use the filename \code{[alignment_id].log},
where \code{alignment_id} is obtained using \link{get_alignment_id}}

\item{log_every}{number of MCMC states between writing to file}

\item{mode}{mode how to log.
Valid values are the ones returned by \link{get_log_modes}}

\item{sanitise_headers}{set to \link{TRUE} to sanitise the headers of the
log file}

\item{sort}{how to sort the log.
Valid values are the ones returned by \link{get_log_sorts}}
}
\value{
a \code{tracelog} object
}
\description{
Create a \code{tracelog} object, as used for testing
}
\examples{
create_test_tracelog()
}
\author{
Richèl J.C. Bilderbeek
}
