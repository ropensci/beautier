#' Internal function
#'
#' Internal function to converts a relaxed log-normal clock model
#' to the \code{prior} section of the XML as text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' @export
rln_clock_model_to_xml_prior_distr <- function( # nolint indeed a long function name
  inference_model
) {
  # Do not be smart yet
  clock_model <- inference_model$clock_model
  testthat::expect_true(beautier::is_rln_clock_model(clock_model))

  text <- NULL

  if (beautier::has_mrca_prior_with_distr(inference_model)) {
    text <- c(
      text,
      beautier::rln_clock_model_to_xml_mean_rate_prior(clock_model)
    )
  }

  id <- clock_model$id
  testit::assert(beautier::is_id(id))
  text <- c(text, paste0("<prior ",
    "id=\"ucldStdevPrior.c:", id, "\" name=\"distribution\" ",
    "x=\"@ucldStdev.c:", id, "\">"))
  text <- c(text,
    beautier::indent(
      beautier::distr_to_xml(
        distr = clock_model$ucldstdev_distr,
        beauti_options = inference_model$beauti_options
      )
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}
