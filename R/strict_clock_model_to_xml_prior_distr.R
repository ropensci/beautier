#' Internal function
#'
#' Internal function to converts a strict clock model
#' to the \code{prior} section of the XML as text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' strict_clock_model_to_xml_prior_distr(
#'   inference_model = create_inference_model()
#' )
#' check_empty_beautier_folder()
#' @export
strict_clock_model_to_xml_prior_distr <- function( # nolint indeed a long internal function name
  inference_model
) {
  testit::assert(beautier::is_strict_clock_model(inference_model$clock_model))

  text <- NULL

  if (inference_model$clock_model$clock_rate_param$estimate == TRUE) {
    clock_model <- inference_model$clock_model
    # No idea why BEAUti does this, see issue_135 files
    if (beautier::is_one_double(clock_model$clock_rate_distr$upper)) {
      clock_model$clock_rate_distr$upper <- Inf
    }

    testthat::expect_true(beautier::is_id(clock_model$id))
    testthat::expect_true(beautier::is_id(clock_model$clock_rate_distr$id))
    opening_tag <- paste0(
      "<prior id=\"ClockPrior.c:", clock_model$id, "\" ",
      "name=\"distribution\" ",
      "x=\"@clockRate.c:", clock_model$id, "\">"
    )
    distr_xml <- beautier::distr_to_xml(
      clock_model$clock_rate_distr,
      beauti_options = inference_model$beauti_options
    )
    closing_tag <- "</prior>"
    text <- c(
      text,
      opening_tag,
      beautier::indent(distr_xml),
      closing_tag
    )
  }

  if (!beautier::is_one_na(inference_model$tipdates_filename)) {
    clock_model <- inference_model$clock_model
    id <- clock_model$id
    testit::assert(beautier::is_id(id))
    text <- c(
      text,
      paste0(
        "<prior id=\"ClockPrior.c:", id, "\" ",
        "name=\"distribution\" x=\"@clockRate.c:", id, "\">"
      )
    )
    text <- c(text,
      beautier::indent(
        beautier::distr_to_xml(
          clock_model$clock_rate_distr,
          beauti_options = inference_model$beauti_options
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  text
}
