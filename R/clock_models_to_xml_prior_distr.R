#' Represent the clock models as XML
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" spec="util.CompoundDistribution" useThreads="true">
#'  #     </distribution>
#'  # </distribution>
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
clock_models_to_xml_prior_distr <- function(clock_models) {
  text <- NULL
  for (clock_model in clock_models) {
    text <- c(
      text,
        clock_model_to_xml_prior_distr(clock_model) # nolint internal function
    )
  }

  # Remove first strict clock
  for (clock_model in clock_models) {
    if (is_strict_clock_model(clock_model)) {
      lines_to_remove <- clock_model_to_xml_prior_distr(clock_model)
      text <- remove_multiline(text, lines_to_remove)
      break
    }
    text
  }

  # Remove first 'meanRate' (in RLN)
  for (clock_model in clock_models) {
    if (is_rln_clock_model(clock_model)) {
      lines_to_remove <- rln_clock_model_to_xml_mean_rate_prior(clock_model)
      text <- remove_multiline(text, lines_to_remove)
      break
    }
    text
  }

  text
}
