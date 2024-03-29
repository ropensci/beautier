% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_alignment_ids.R
\name{get_alignment_ids_from_fasta_filenames}
\alias{get_alignment_ids_from_fasta_filenames}
\title{Get the alignment ID from one or more FASTA filenames.}
\usage{
get_alignment_ids_from_fasta_filenames(fasta_filenames)
}
\arguments{
\item{fasta_filenames}{One or more FASTA filenames.
Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.}
}
\value{
the IDs from one or more FASTA files
}
\description{
This is done in the same way as BEAST2 does by default.
The files are assumed to be FASTA. If this is not the case, there
may be any kind of error message when calling this function.
}
\examples{
check_empty_beautier_folder()

get_alignment_ids_from_fasta_filenames(
  get_beautier_paths(c("anthus_aco.fas", "anthus_nd2.fas"))
)

check_empty_beautier_folder()
}
\seealso{
Use \link{get_alignment_ids} to get the alignment IDs from multiple
kids of files.
Use \link{are_fasta_filenames} to
see if the filenames all have a common FASTA filename extension.
}
\author{
Richèl J.C. Bilderbeek
}
