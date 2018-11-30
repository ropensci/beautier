#' Creates the MRCA priors' XML for the tracelog section
#' @inheritParams default_params_doc
#' @seealso the complete tracelog section is created
#'   by \code{\link{create_beast2_input_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richel J.C. Bilderbeek
#' @noRd
mrca_priors_to_xml_tracelog <- function(
  clock_models,
  mrca_priors
) {
  has_non_strict_clock_model <- is_rln_clock_model(clock_models[[1]]) # nolint internal function

  testit::assert(are_mrca_priors(mrca_priors)) # nolint internal function
  text <- NULL

  for (mrca_prior in mrca_priors) {
    text <- c(text,
      mrca_prior_to_xml_tracelog(
        mrca_prior = mrca_prior,
        has_non_strict_clock_model = has_non_strict_clock_model
      )
    )
  }
  text
}
