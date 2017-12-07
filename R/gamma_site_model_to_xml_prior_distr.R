#' Creates the gamma site models section in the distribution section
#' of a BEAST2 XML parameter file
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
gamma_site_model_to_xml_prior_distr <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  gamma_site_model <- beautier::get_gamma_site_model(
    site_model = site_model)
  if (beautier::get_gamma_cat_count(gamma_site_model) >= 2) {
    text <- c(text, paste0("<prior ",
      "id=\"GammaShapePrior.s:", id, "\" name=\"distribution\" ",
      "x=\"@gammaShape.s:", id, "\">"))
    text <- c(
      text,
      beautier::indent(
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
