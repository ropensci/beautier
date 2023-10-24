#' Converts a site model to XML,
#'   used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
tn93_site_model_to_xml_state <- function(
  site_model,
  beauti_options = create_beauti_options()
) {
  check_true(is_site_model(site_model))
  check_beauti_options(beauti_options)
  id <- site_model$id
  check_true(is_id(id))
  text <- NULL
  check_true(is_tn93_site_model(site_model))

  # Indeed, overwrite by the site model's ID
  site_model$kappa_1_param$id <- id
  site_model$kappa_2_param$id <- id
  site_model$freq_param$id <- id


  if (site_model$kappa_1_param$estimate == TRUE) {
    text <- c(
      text,
      parameter_to_xml(
        site_model$kappa_1_param,
        beauti_options = beauti_options
      )
    )
  }
  if (site_model$kappa_2_param$estimate == TRUE) {
    text <- c(
      text,
      parameter_to_xml(
        site_model$kappa_2_param,
        beauti_options = beauti_options
      )
    )
  }
  text <- c(
    text,
    freq_param_to_xml(
      site_model$freq_param,
      beauti_options = beauti_options
    )
  )

  text <- c(
    text,
    gamma_site_model_to_xml_state(site_model$gamma_site_model, id)
  )
  text
}
