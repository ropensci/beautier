% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_site_models.R
\name{create_site_models}
\alias{create_site_models}
\title{Creates all supported site models
  which is a list of the types returned by
  \code{\link{create_gtr_site_model}},
  \code{\link{create_hky_site_model}},
  \code{\link{create_jc69_site_model}}
  and \code{\link{create_tn93_site_model}}}
\usage{
create_site_models()
}
\value{
a list of site_models
}
\description{
Creates all supported site models
  which is a list of the types returned by
  \code{\link{create_gtr_site_model}},
  \code{\link{create_hky_site_model}},
  \code{\link{create_jc69_site_model}}
  and \code{\link{create_tn93_site_model}}
}
\examples{
check_empty_beautier_folder()

# All created site models are a kind of site model
site_models <- create_site_models()

# TRUE
is_gtr_site_model(site_models[[1]])
is_hky_site_model(site_models[[2]])
is_jc69_site_model(site_models[[3]])
is_tn93_site_model(site_models[[4]])

check_empty_beautier_folder()
}
\seealso{
Use \link{create_site_model} to create a site model
}
\author{
Richèl J.C. Bilderbeek
}
