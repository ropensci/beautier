% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/are_equal_treelogs.R
\name{are_equal_treelogs}
\alias{are_equal_treelogs}
\title{Determine if two treelogs are equal.}
\usage{
are_equal_treelogs(treelog_1, treelog_2)
}
\arguments{
\item{treelog_1}{an treelog, as created by \link{create_treelog}}

\item{treelog_2}{an treelog, as created by \link{create_treelog}}
}
\value{
TRUE if the two treelogs are equal
}
\description{
Will \link{stop} if the arguments are not treelogs.
}
\examples{
check_empty_beautier_folder()

treelog_1 <- create_treelog(log_every = 1000)
treelog_2 <- create_treelog(log_every = 314)
# TRUE
are_equal_treelogs(treelog_1, treelog_1)
# FALSE
are_equal_treelogs(treelog_1, treelog_2)

check_empty_beautier_folder()
}
\seealso{
Use \link{create_treelog} to create an treelog
}
\author{
Richèl J.C. Bilderbeek
}
