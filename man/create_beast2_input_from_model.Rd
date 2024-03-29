% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_beast2_input_from_model.R
\name{create_beast2_input_from_model}
\alias{create_beast2_input_from_model}
\title{Create a BEAST2 XML input text from an inference model}
\usage{
create_beast2_input_from_model(input_filename, inference_model)
}
\arguments{
\item{input_filename}{A FASTA filename.
Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.}

\item{inference_model}{a Bayesian phylogenetic inference model.
An inference model is the complete model setup in which a site model,
clock model, tree prior and more are specified.
Use \link{create_inference_model} to create an inference model.
Use \link{check_inference_model} to check if  an inference model is valid.
Use \link{rename_inference_model_filenames} to rename the files in an
inference model.}
}
\value{
a character vector of XML strings
}
\description{
The main two XML tags are these:
\preformatted{
  <?xml[...]?><beast[...]>
  [...]
  </beast>
}
}
\examples{
if (is_on_ci()) {

  check_empty_beautier_folder()

  text <- create_beast2_input_from_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model()
  )

  check_empty_beautier_folder()
}
}
\seealso{
Use \link{create_beast2_input_file_from_model} to also save it to file.
Use \link{create_xml_declaration}
to create the XML text of the XML declaration.
Use \link{create_beast2_input_beast} to create
to create the XML text of the \code{beast} tag.
}
\author{
Richèl J.C. Bilderbeek
}
