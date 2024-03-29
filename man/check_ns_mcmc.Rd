% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_mcmc_nested_sampling.R
\name{check_ns_mcmc}
\alias{check_ns_mcmc}
\alias{check_mcmc_nested_sampling}
\alias{check_nested_sampling_mcmc}
\title{Check if this an MCMC that uses Nested Sampling
to estimate a marginal likelihood.}
\usage{
check_ns_mcmc(mcmc)
}
\arguments{
\item{mcmc}{one MCMC.
Use \code{\link{create_mcmc}} to create an MCMC.
Use \code{\link{create_ns_mcmc}} to create an MCMC
  for a Nested Sampling run.
Use \code{\link{check_mcmc}} to check if an MCMC is valid.
Use \code{\link{rename_mcmc_filenames}} to rename the filenames in an MCMC.}
}
\value{
No return value, called for side effects
}
\description{
Will \link{stop} if not, else will do nothing
}
\seealso{
use \code{\link{create_ns_mcmc}}
to create an MCMC that uses Nested Sampling
to estimate a marginal likelihood
}
\author{
Richèl J.C. Bilderbeek
}
