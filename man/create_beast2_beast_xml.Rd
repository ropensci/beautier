% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_beast2_beast_xml.R
\name{create_beast2_beast_xml}
\alias{create_beast2_beast_xml}
\title{Create the \code{<beast ...>} XML}
\usage{
create_beast2_beast_xml(beauti_options)
}
\arguments{
\item{beauti_options}{one BEAUti options object,
as returned by \code{\link{create_beauti_options}}}
}
\value{
the XML
}
\description{
The \code{<beast ...>} XML is the XML at the start of a BEAST2
XML input file, directly after the general XML declaration (as
created by \link{create_xml_declaration}).
}
\examples{
remove_beautier_folder()

create_beast2_beast_xml(
  beauti_options = create_beauti_options_v2_6()
)

check_empty_beautier_folder()
}
\author{
Richèl J.C. Bilderbeek
}
