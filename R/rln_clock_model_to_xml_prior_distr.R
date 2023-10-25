#' Internal function
#'
#' Internal function to converts a relaxed log-normal clock model
#' to the \code{prior} section of the XML as text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#'
#' # Must be an initialized inference model
#' inference_model <- create_inference_model(
#'   clock_model = create_rln_clock_model(
#'     id = "test_output_0",
#'     ucldstdev_distr = create_gamma_distr(
#'       id = 0,
#'       alpha = create_alpha_param(id = 2, value = "0.5396"),
#'       beta = create_beta_param(id = 3, value = "0.3819")
#'     ),
#'     mean_rate_prior_distr = create_uniform_distr(id = 1),
#'     mparam_id = 1
#'   )
#' )
#'
#' rln_clock_model_to_xml_prior_distr(inference_model)
#'
#' check_empty_beautier_folder()
#' @export
rln_clock_model_to_xml_prior_distr <- function( # nolint indeed a long function name
  inference_model
) {
  # Do not be smart yet
  clock_model <- inference_model$clock_model
  check_true(is_rln_clock_model(clock_model))

  text <- NULL

  if (has_mrca_prior_with_distr(inference_model)) {
    text <- c(
      text,
      rln_clock_model_to_xml_mean_rate_prior(
        clock_model,
        beauti_options = inference_model$beauti_options
      )
    )
  }

  id <- clock_model$id
  check_true(is_id(id))
  text <- c(
    text,
    paste0(
      "<prior ",
      "id=\"ucldStdevPrior.c:", id, "\" name=\"distribution\" ",
      "x=\"@ucldStdev.c:", id, "\">"
    )
  )
  text <- c(text,
    indent(
      distr_to_xml(
        distr = clock_model$ucldstdev_distr,
        beauti_options = inference_model$beauti_options
      )
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}
