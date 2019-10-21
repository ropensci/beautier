% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc_to_xml_run.R
\name{mcmc_to_xml_run}
\alias{mcmc_to_xml_run}
\title{Converts an MCMC object to the run section's XML}
\usage{
mcmc_to_xml_run(mcmc)
}
\arguments{
\item{mcmc}{one MCMC
as returned by \code{\link{create_mcmc}}}
}
\value{
the XML as text
}
\description{
Converts an MCMC object to the run section's XML
}
\examples{
library(testthat)

xml <- mcmc_to_xml_run(create_mcmc())
expect_equal(
  xml,
  "<run id=\\"mcmc\\" spec=\\"MCMC\\" chainLength=\\"1e+07\\">"
)
}
\author{
Richèl J.C. Bilderbeek
}