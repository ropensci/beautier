#' Internal function
#'
#' Internal function to converts a clock model
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
#' check_empty_beautier_folder()
#' @export
clock_model_to_xml_prior_distr <- function(
  inference_model
) {
  # Do not be smart yet
  clock_model <- inference_model$clock_model
  tipdates_filename <- inference_model$tipdates_filename

  testit::assert(beautier::is_clock_model(clock_model))

  text <- NULL
  if (beautier::is_rln_clock_model(clock_model)) {
    return(beautier::rln_clock_model_to_xml_prior_distr(inference_model))
  } else {
    # Fails for unimplemented clock models
    testit::assert(beautier::is_strict_clock_model(clock_model))

    if (!beautier::is_one_na(tipdates_filename)) {
      id <- clock_model$id
      testit::assert(beautier::is_id(id))
      text <- c(text, paste0("<prior id=\"ClockPrior.c:", id, "\" ",
        "name=\"distribution\" x=\"@clockRate.c:", id, "\">"))
      text <- c(text,
        beautier::indent(
          beautier::distr_to_xml(
            clock_model$clock_rate_distr
          )
        )
      )
      text <- c(text, paste0("</prior>"))
    }
  }
  text
}
