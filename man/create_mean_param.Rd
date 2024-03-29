% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_param.R
\name{create_mean_param}
\alias{create_mean_param}
\alias{create_param_mean}
\title{Create a parameter called mean}
\usage{
create_mean_param(id = NA, value = 0)
}
\arguments{
\item{id}{the parameter's ID}

\item{value}{value of the parameter}
}
\value{
a parameter called mean
}
\description{
Create a parameter called mean
}
\note{
this parameter is used in an exponential distribution
  (as returned by \code{\link{create_exp_distr}})
  and normal distribution
  (as returned by \code{\link{create_normal_distr}}).
  It cannot be estimated (as a hyper parameter) yet.
}
\examples{
if (is_on_ci()) {

  # Create the parameter
  mean_param <- create_mean_param(value = 1.0)

  # Use the parameter in a distribution
  exp_distr <- create_exp_distr(
    mean = mean_param
  )

  # Use the distribution to create a BEAST2 input file
  beast2_input_file <- get_beautier_tempfilename()
  create_beast2_input_file(
    input_filename = get_fasta_filename(),
    beast2_input_file,
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = exp_distr
    )
  )
  file.remove(beast2_input_file)

  remove_beautier_folder()
}
}
\seealso{
the function \code{\link{create_param}} contains a list
  of all parameters that can be created
}
\author{
Richèl J.C. Bilderbeek
}
