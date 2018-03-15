#' Creates the XML of a list of one or more MRCA priors,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the MRCA priors as XML text
#' @author Richel J.C. Bilderbeek
mrca_priors_to_xml_operators <- function(
  mrca_priors,
  has_non_strict_clock_model = FALSE
) {

  testit::assert(are_mrca_priors(mrca_priors))

  text <- NULL
  is_first <- TRUE
  for (mrca_prior in mrca_priors) {

    text <- c(
      text,
      mrca_prior_to_xml_operators(
        mrca_prior = mrca_prior,
        has_non_strict_clock_model = has_non_strict_clock_model,
        is_first = is_first
      )
    )
    is_first <- FALSE
  }
  text
}
