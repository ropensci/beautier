gamma_site_models_to_xml_prior_distr <- function(site_models)
{
  text <- NULL
  for (site_model in site_models) {
    text <- c(
      text,
      gamma_site_model_to_xml_prior_distr(site_model) # nolint
    )
  }
  text
}
