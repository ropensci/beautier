#' Represent the clock models as XML
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
clock_models_to_xml_prior_distr <- function(clock_models) { # nolint internal function name
  text <- NULL
  for (clock_model in clock_models) {
    text <- c(
      text,
        clock_model_to_xml_prior_distr(clock_model) # nolint internal function
    )
  }

  # Remove first clock rate, which is
  #  - for strict: <prior id=\"ClockPrior.c:...
  #  - for RLN: <prior id=\"MeanRatePrior.c:...
  clock_model <- clock_models[[1]]
  if (is_strict_clock_model(clock_model)) {
    lines_to_remove <- clock_model_to_xml_prior_distr(clock_model) # nolint internal function
    text <- remove_multiline(text, lines_to_remove) # nolint internal function
  } else {
    # Will fail for unimplemented clock models
    testit::assert(beautier::is_rln_clock_model(clock_model))

    lines_to_remove <- rln_clock_model_to_xml_mean_rate_prior(clock_model) # nolint internal function
    text <- remove_multiline(text, lines_to_remove) # nolint internal function
  }

  text
}
