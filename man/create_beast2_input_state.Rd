% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_beast2_input_state.R
\name{create_beast2_input_state}
\alias{create_beast2_input_state}
\title{Creates the '\code{state}' section of a BEAST2 XML parameter file}
\usage{
create_beast2_input_state(inference_model)
}
\arguments{
\item{inference_model}{a Bayesian phylogenetic inference model.
An inference model is the complete model setup in which a site model,
clock model, tree prior and more are specified.
Use \link{create_inference_model} to create an inference model.
Use \link{check_inference_model} to check if  an inference model is valid.
Use \link{rename_inference_model_filenames} to rename the files in an
inference model.}
}
\value{
lines of XML text
}
\description{
Creates the '\code{state}' section of a BEAST2 XML parameter file,
without being indented.
}
\details{
The \code{state} tag has these elements:
\preformatted{
   <state[...]>
       <tree[...]>
       [...]
       </tree>
       [parameters]
    </run>
}
}
\seealso{
Use \link{create_beast2_input_state}
to create the XML text of the \code{tree} tag.
to create the XML text of the \code{[parameters]} section.
}
\author{
Richèl J.C. Bilderbeek
}
