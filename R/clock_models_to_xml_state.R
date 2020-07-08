#' Converts one or more clock models to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
clock_models_to_xml_state <- function(
  inference_model,
  clock_models = "deprecated",
  mrca_priors = "deprecated",
  has_tip_dating = "deprecated"
) {
  beautier::clock_models_to_xml_state_check_deprecated( # nolint indeed a long function name
    clock_models = clock_models,
    mrca_priors = mrca_priors,
    has_tip_dating = has_tip_dating
  )

  # Do not be smart yet
  clock_models <- list(inference_model$clock_model)
  mrca_priors <- list(inference_model$mrca_prior)
  has_tip_dating <- !beautier::is_one_na(inference_model$tipdates_filename)

  # the mrca_priors are supposed to be temporary :-)
  testit::assert(beautier::are_clock_models(clock_models))

  if (length(clock_models) == 1 &&
      beautier::is_strict_clock_model(clock_models[[1]]) &&
    has_tip_dating == FALSE
  ) {
    return(NULL)
  }

  text <- NULL

  for (clock_model in clock_models) {
    text <- c(text,
      clock_model_to_xml_state(
        clock_model = clock_model,
        has_tip_dating = has_tip_dating
      )
    )
  }

  # Remove the first line of the first clock model,
  # if no MRCA prior with a distribution is used
  if (beautier::is_rln_clock_model(clock_models[[1]]) &&
      !beautier::is_mrca_prior_with_distr(mrca_priors[[1]])) {
    # A RLN clock model returns three lines, only remove the first
    line_to_remove <- beautier::clock_model_to_xml_state(clock_models[[1]])
    testit::assert(length(line_to_remove) == 3)
    text <- line_to_remove[
      stringr::str_remove_all(
        string = line_to_remove, pattern = ".*ucldMean\\.c:.*"
      ) != ""
    ]
  }
  text
}


#' Internal function to check if \link{clock_models_to_xml_state}
#' uses deprecated arguments.
#'
#' This internal function checks if \link{clock_models_to_xml_state}
#' uses deprecated arguments.
#' Will \link{stop} if this is the case
#' @inheritParams default_params_doc
#' @return Nothing
#' @author Richèl J.C. Bilderbeek
#' @export
clock_models_to_xml_state_check_deprecated <- function( # nolint indeed a long function name
  clock_models,
  mrca_priors,
  has_tip_dating
) {
  if (clock_models != "deprecated") {
    stop("'clock_models' is deprecated, use 'inference_model' instead")
  }
  if (mrca_priors != "deprecated") {
    stop("'mrca_priors' is deprecated, use 'inference_model' instead")
  }
  if (has_tip_dating != "deprecated") {
    stop("'has_tip_dating' is deprecated, use 'inference_model' instead")
  }
}
