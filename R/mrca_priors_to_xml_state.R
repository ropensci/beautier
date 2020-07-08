#' Converts one or more MRCA priors to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
mrca_priors_to_xml_state <- function(
  inference_model,
  mrca_priors = "deprecated",
  has_non_strict_clock_model = "deprecated"
) {
  if (mrca_priors != "deprecated") {
    stop("'mrca_priors' is deprecated, use 'inference_model' instead")
  }
  if (has_non_strict_clock_model != "deprecated") {
    stop(
      "'has_non_strict_clock_model' is deprecated, ",
      "use 'inference_model' instead"
    )
  }

  # Do not be smart yet
  mrca_priors <- list(inference_model$mrca_prior)
  has_non_strict_clock_model <- get_has_non_strict_clock_model(
    list(inference_model$clock_model)
  )

  if (length(mrca_priors) == 1 && beautier::is_one_na(mrca_priors)) {
    return(NULL)
  }

  text <- NULL
  for (mrca_prior in mrca_priors) {
    text <- c(text,
      mrca_prior_to_xml_state(
        mrca_prior = mrca_prior,
        has_non_strict_clock_model = has_non_strict_clock_model
      )
    )
  }

  text
}
