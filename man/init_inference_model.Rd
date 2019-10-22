% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init_inference_model.R
\name{init_inference_model}
\alias{init_inference_model}
\title{Initialize an inference model}
\usage{
init_inference_model(input_filename, inference_model)
}
\arguments{
\item{input_filename}{A FASTA filename.
Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.}

\item{inference_model}{an Bayesian phylogenetic inference model,
as can be created by \code{\link{create_inference_model}}.
An inference model is the complete model setup in which a site model,
clock model, tree prior and more are specified.}
}
\description{
Initialize an inference model
}