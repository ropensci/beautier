% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_data_xml.R
\name{create_data_xml}
\alias{create_data_xml}
\title{Create the \code{<data ..>} XML}
\usage{
create_data_xml(id, beast2_version)
}
\arguments{
\item{id}{an alignment's IDs.
An ID can be extracted from its FASTA filename
with \code{\link{get_alignment_ids_from_fasta_filenames}})}

\item{beast2_version}{BEAST2 version, for example, \code{"2.5"}}
}
\value{
lines of XML text
}
\description{
Create the \code{<data ..>} XML
}
\author{
Richèl J.C. Bilderbeek
}
