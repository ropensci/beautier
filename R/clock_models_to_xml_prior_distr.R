clock_models_to_xml_prior_distr <- function(clock_models) {
  text <- NULL
  for (clock_model in clock_models) {
    text <- c(
      text,
        clock_model_to_xml_prior_distr(clock_model) # nolint internal function
    )
  }

  # Remove first 'meanRate'
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
