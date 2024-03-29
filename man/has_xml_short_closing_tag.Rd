% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/has_xml_short_closing_tag.R
\name{has_xml_short_closing_tag}
\alias{has_xml_short_closing_tag}
\title{Is an XML closing tag with short closing text in
one of the lines of the text?}
\usage{
has_xml_short_closing_tag(lines)
}
\arguments{
\item{lines}{lines of an XML text}
}
\value{
TRUE if there is an XML tag that also closes present in the lines
  of text, FALSE otherwise
}
\description{
Is an XML closing tag with short closing text in
one of the lines of the text?
}
\examples{
check_empty_beautier_folder()

# TRUE
has_xml_short_closing_tag("<my_tag id=1/>")
# FALSE
has_xml_short_closing_tag("<my_tag id=1>text</my_tag>")

check_empty_beautier_folder()
}
\author{
Richèl J.C. Bilderbeek
}
