#' Creates the XML of a list of one or more MRCA priors,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the MRCA priors as XML text
#' @author Richel J.C. Bilderbeek
#' @noRd
mrca_priors_to_xml_operators <- function(
  mrca_priors
) {

  testit::assert(are_mrca_priors(mrca_priors))

  text <- NULL
  for (mrca_prior in mrca_priors) {

    text <- c(
      text,
      mrca_prior_to_xml_operators(
        mrca_prior = mrca_prior
      )
    )
  }
  text
}
