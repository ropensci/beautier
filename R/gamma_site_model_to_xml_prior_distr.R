#' Creates the gamma site models section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
gamma_site_model_to_xml_prior_distr <- function( # nolint internal function
  site_model
) {
  testit::assert(is_site_model(site_model))
  id <- site_model$id
  testit::assert(is_id(id))

  text <- NULL
  gamma_site_model <- site_model$gamma_site_model
  if (gamma_site_model$gamma_cat_count >= 2) {
    text <- c(text, paste0("<prior ",
      "id=\"GammaShapePrior.s:", id, "\" name=\"distribution\" ",
      "x=\"@gammaShape.s:", id, "\">"))
    text <- c(
      text,
      indent(
        distr_to_xml(
          gamma_site_model$gamma_shape_prior_distr
        ),
        n_spaces = 4
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  text

}
