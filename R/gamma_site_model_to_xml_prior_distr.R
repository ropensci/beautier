#' Internal function.
#'
#' Creates the gamma site models section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
gamma_site_model_to_xml_prior_distr <- function( # nolint indeed long function name
  inference_model
) {
  beautier::check_inference_model(inference_model)
  site_model <- inference_model$site_model # don't be smart yet
  beauti_options <- inference_model$beauti_options  # don't be smart yet
  id <- site_model$id

  text <- NULL

  if (inference_model$beauti_options$beast2_version == "2.6" &&
      !beautier::is_jc69_site_model(inference_model$site_model)
  ) {
    testthat::expect_true(
      beautier::is_id(
        inference_model$site_model$gamma_site_model$freq_prior_uniform_distr_id
      )
    )

    text <- c(
      text,
      paste0(
        "<prior ",
        "id=\"FrequenciesPrior.s:", id, "\" ",
        "name=\"distribution\" ",
        "x=\"@freqParameter.s:", id,
        "\">"
      ),
      beautier::indent(
        paste0(
          "<Uniform ",
          "id=\"Uniform.", inference_model$site_model$gamma_site_model$freq_prior_uniform_distr_id, "\" ", # nolint indeed a long line
          "name=\"distr\"",
          "/>"
        )
      ),
      "</prior>"
    )
  }

  gamma_site_model <- site_model$gamma_site_model
  if (gamma_site_model$gamma_cat_count >= 2) {
    text <- c(
      text,
      paste0(
        "<prior ",
        "id=\"GammaShapePrior.s:", id, "\" name=\"distribution\" ",
        "x=\"@gammaShape.s:", id, "\">"
      )
    )
    text <- c(
      text,
      beautier::indent(
        beautier::distr_to_xml(
          gamma_site_model$gamma_shape_prior_distr,
          beauti_options = beauti_options
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  text

}
