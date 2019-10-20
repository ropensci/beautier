#' Determines if the inference model has an MRCA prior.
#'
#' Will \link{stop} if the inference model is invalid
#' @inheritParams default_params_doc
#' @note MRCA: 'Most Recent Common Ancestor'
#' @return TRUE if the inference model has an MRCA prior,
#'   FALSE otherwise
#' @examples
#'   # No MRCA prior
#'   inference_model <- create_inference_model(
#'     mrca_prior = NA
#'   )
#'   testthat::expect_false(has_mrca_prior(inference_model))
#'
#'   # A default MRCA prior
#'   inference_model <- create_inference_model(
#'     mrca_prior = create_mrca_prior()
#'   )
#'   testthat::expect_true(has_mrca_prior(inference_model))
#' @seealso
#'   \itemize{
#'     \item \code{\link{create_inference_model}}: create an inference model
#'     \item \code{\link{create_mrca_prior}}: create an MRCA prior
#'   }
#' @author Richèl J.C. Bilderbeek
#' @export
has_mrca_prior <- function(
  inference_model
) {
  beautier::check_inference_model(inference_model)
  !beautier::is_one_na(inference_model$mrca_prior)
}
