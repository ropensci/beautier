#' Internal function to creates the XML text for the
#' \code{parameter}s within the \code{siteModel} section
#' of a BEAST2 parameter file.
#'
#' Internal function to creates the XML text for the
#' \code{parameter}s within the \code{siteModel} section,
#' which is part of the \code{siteModel} section
#' of a BEAST2 parameter file.
#'
#' The \code{parameter}s sections has these elements:
#'
#' \preformatted{
#'    [parameters]
#' }
#'
#' \code{[parameters]} can be a combination of these:
#'
#' \preformatted{
#'   <parameter id="mutationRate.s[...]>
#'   <parameter id="gammaShape.s[...]>
#'   <parameter id="proportionInvariant.s[...]>
#' }
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_site_model_parameters_xml <- function( # nolint long function name, which is fine for an internal function
  inference_model
) {
  id <- inference_model$site_model$id
  testit::assert(beautier::is_id(id))

  # gcc: gamma category count
  gcc <- inference_model$site_model$gamma_site_model$gamma_cat_count

  # mutationRate parameter
  mutation_rate_parameter <- paste0("<parameter ",
    "id=\"mutationRate.s:", id, "\" "
  )
  if (inference_model$beauti_options$beast2_version == "2.6") {
    mutation_rate_parameter <- paste0(mutation_rate_parameter,
      "spec=\"parameter.RealParameter\" "
    )
  }
  mutation_rate_parameter <- paste0(mutation_rate_parameter,
    "estimate=\"false\" name=\"mutationRate\">1.0</parameter>"
  )

  # gammaShape if needed
  gamma_shape_parameter <- NULL
  if (gcc < 2) {
    gamma_shape_parameter <- paste0(
      "<parameter ",
      "id=\"gammaShape.s:", id, "\" "
    )
    if (inference_model$beauti_options$beast2_version == "2.6") {
      gamma_shape_parameter <- paste0(gamma_shape_parameter,
        "spec=\"parameter.RealParameter\" "
      )
    }

    gamma_shape_parameter <- paste0(gamma_shape_parameter,
      "estimate=\"false\" name=\"shape\">1.0</parameter>"
    )
  }

  # proportionInvariant parameter
  proportion_invariant_parameter <- paste0(
    "<parameter id=\"proportionInvariant.s:", id, "\" ")
  if (inference_model$beauti_options$beast2_version == "2.6") {
    proportion_invariant_parameter <- paste0(proportion_invariant_parameter,
      "spec=\"parameter.RealParameter\" "
    )
  }
  proportion_invariant_parameter <- paste0(
    proportion_invariant_parameter,
    "estimate=\"false\" lower=\"0.0\" ",
    "name=\"proportionInvariant\" upper=\"1.0\">",
    inference_model$site_model$gamma_site_model$prop_invariant,
    "</parameter>"
  )

  # Layout of the text
  text <- mutation_rate_parameter
  if (!is.null(gamma_shape_parameter)) {
    text <- c(text, gamma_shape_parameter)
  }
  text <- c(
    text,
    paste0(proportion_invariant_parameter)
  )

  if (inference_model$beauti_options$beast2_version == "2.6") {
    text <- rep(text, each = 2)
    text[seq(2, length(text), by = 2)] <- "                    "
  }
  text
}
