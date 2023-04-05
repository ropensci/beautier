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
  # Do not be smart yet
  clock_model <- inference_model$clock_model
  tipdates_filename <- inference_model$tipdates_filename

  testit::assert(beautier::is_strict_clock_model(clock_model))

  text <- NULL

  if (clock_model$clock_rate_param$estimate == TRUE) {
    testthat::expect_true(beautier::is_id(clock_model$id))
    testthat::expect_true(beautier::is_id(clock_model$clock_rate_distr$id))
    text <- c(
      text,
      paste0(
        "<prior id=\"ClockPrior.c:", clock_model$id, "\" ",
        "name=\"distribution\" ",
        "x=\"@clockRate.c:", clock_model$id, "\">"
      ),
      beautier::indent(
        beautier::distr_to_xml(
          clock_model$clock_rate_distr,
          beauti_options = inference_model$beauti_options
        )
      ),
      "</prior>"
    )
  }

  if (!beautier::is_one_na(tipdates_filename)) {
    id <- clock_model$id
    testit::assert(beautier::is_id(id))
    text <- c(text, paste0("<prior id=\"ClockPrior.c:", id, "\" ",
      "name=\"distribution\" x=\"@clockRate.c:", id, "\">"))
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
