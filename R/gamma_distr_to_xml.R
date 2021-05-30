#' Internal function
#'
#' Converts a gamma distribution to XML
#' @inheritParams default_params_doc
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
gamma_distr_to_xml <- function(
  gamma_distr,
  beauti_options = create_beauti_options()
) {
  # Don't be smart yet
  distr <- gamma_distr

  testit::assert(beautier::is_gamma_distr(distr))
  beautier::check_beauti_options(beauti_options)
  id <- distr$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<Gamma ",
    "id=\"Gamma.", id, "\" name=\"distr\">"))
  text <- c(text,
    beautier::indent(
      beautier::alpha_parameter_to_xml(
        alpha_parameter = distr$alpha,
        beauti_options = beauti_options
      )
    )
  )
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(
        parameter = distr$beta,
        beauti_options = beauti_options
      )
    )
  )
  text <- c(text, paste0("</Gamma>"))
  text
}
