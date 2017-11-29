#' Converts a site model to XML,
#'   used in the \code{operators} section
#' @param site_model a site model,
#'   as created by \code{\link{create_site_model}})
#' @return the site model as XML text
#' @author Richel J.C. Bilderbeek
site_model_to_xml_operators <- function(
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL

  # There are three parts: rate, freq and gamma. Order differs
  gamma_shape_scaler <- create_beast2_input_operators_gamma_shape_scaler(site_model = site_model) # nolint
  frequencies_exchanger <- create_beast2_input_operators_frequencies_exchanger(site_model = site_model) # nolint
  rates <- create_beast2_input_operators_rates(site_model = site_model) # nolint
  gcc <- beautier::get_gamma_cat_count(beautier::get_gamma_site_model(site_model)) # nolint
  prop_invariant <- beautier::get_prop_invariant(beautier::get_gamma_site_model(site_model)) # nolint

  if (is_gtr_site_model(site_model)) {
    if (gcc == 0) {
      text <- c(text, rates)
      text <- c(text, frequencies_exchanger)
    } else if (gcc == 1) {
      text <- c(text, frequencies_exchanger)
      text <- c(text, rates)
    } else {
      if (prop_invariant == get_default_prop_invariant()) {
        text <- c(text, frequencies_exchanger)
        text <- c(text, rates)
        text <- c(text, gamma_shape_scaler)
      } else {
        text <- c(text, gamma_shape_scaler)
        text <- c(text, frequencies_exchanger)
        text <- c(text, rates)
      }
    }
  } else {
    text <- c(text, rates)
    text <- c(text, gamma_shape_scaler)
    text <- c(text, frequencies_exchanger)
  }

  text
}
