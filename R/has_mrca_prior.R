#' Determines if the inference model has an MRCA prior.
#'
#' Will \link{stop} if the inference model is invalid
#' @inheritParams default_params_doc
#' @note MRCA: 'Most Recent Common Ancestor'
#' @return TRUE if the inference model has an MRCA prior,
#'   FALSE otherwise
#' @examples
#' check_empty_beautier_folder()
#'
#' # No MRCA prior
#' inference_model <- create_inference_model(
#'   mrca_prior = NA
#' )
#' has_mrca_prior(inference_model) # Returns FALSE
#'
#' # A default MRCA prior
#' inference_model <- create_inference_model(
#'   mrca_prior = create_mrca_prior()
#' )
#' has_mrca_prior(inference_model) # Returns TRUE
#'
#' check_empty_beautier_folder()
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
  check_inference_model(inference_model)
  !is_one_na(inference_model$mrca_prior)
}
