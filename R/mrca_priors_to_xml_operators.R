#' Creates the XML of a list of one or more MRCA priors,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the MRCA priors as XML text
#' @author Richel J.C. Bilderbeek
#' @noRd
mrca_priors_to_xml_operators <- function(
  mrca_priors,
  clock_models
) {
  testit::assert(are_mrca_priors(mrca_priors)) # nolint internal function
  testit::assert(are_clock_models(clock_models)) # nolint internal function

  text <- NULL
  for (mrca_prior in mrca_priors) {

    text <- c(
      text,
      mrca_prior_to_xml_operators(
        mrca_prior = mrca_prior,
        fixed_crown_age = FALSE,
        clock_models = clock_models
      )
    )
  }
  text
}
