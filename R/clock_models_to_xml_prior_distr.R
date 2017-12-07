clock_models_to_xml_prior_distr <- function(clock_models) {
  text <- NULL
  for (clock_model in clock_models) {
    text <- c(
      text,
        clock_model_to_xml_prior_distr(clock_model) # nolint internal function
    )
  }
  text
}