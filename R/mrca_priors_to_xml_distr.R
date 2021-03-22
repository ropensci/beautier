#' Creates the the \code{distribution}'s prior section (which is part of
#' a posterior distribution section) of a BEAST2 XML parameter file.
#'
#' These lines start with '\code{<distribution id="prior"}'
#'
#' \code{
#'    <distribution id="posterior" spec="util.CompoundDistribution">
#'        <distribution id="prior" spec="util.CompoundDistribution">
#'          HERE, where the ID of the distribution is 'prior'
#'        </distribution>
#'        <distribution id="likelihood" ...>
#'        </distribution>
#'   </distribution>
#' }
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
mrca_priors_to_xml_prior_distr <- function(
  inference_model,
  mrca_priors = "deprecated",
  has_non_strict_clock_model = "deprecated"
) {
  if (mrca_priors != "deprecated") {
    stop("'mrca_priors' is deprecated. Use 'inference_model' instead")
  }
  if (has_non_strict_clock_model != "deprecated") {
    stop(
      "'has_non_strict_clock_model' is deprecated, ",
      "it is extracted from 'inference_model'"
    )
  }
  # Don't be smart yet
  mrca_priors <- list(inference_model$mrca_prior)
  testit::assert(beautier::are_mrca_priors(mrca_priors))

  if (beautier::is_one_na(mrca_priors)) return(NULL)
  beautier::mrca_prior_to_xml_prior_distr(
    inference_model = inference_model
  )
}
