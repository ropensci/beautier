#' Internal function
#'
#' Converts a TN93 site model to XML,
#'   used in the \code{prior} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @examples
#' tn93_site_model_to_xml_prior_distr(
#'   site_model = create_tn93_site_model(
#'     id = 1,
#'     kappa_1_prior_distr = create_uniform_distr(id = 2),
#'     kappa_2_prior_distr = create_uniform_distr(id = 3)
#'   ),
#'   beauti_options = create_beauti_options()
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
tn93_site_model_to_xml_prior_distr <- function( # nolint indeed a long internal function name
  site_model,
  beauti_options
) {
  testthat::expect_true(beautier::is_tn93_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))
  text <- NULL
  if (site_model$kappa_1_param$estimate == TRUE) {
    text <- c(text, paste0("<prior id=\"kappa1Prior.s:", id, "\" ",
                           "name=\"distribution\" x=\"@kappa1.s:", id, "\">"))
    text <- c(
      text,
      beautier::indent(
        beautier::distr_to_xml(
          site_model$kappa_1_prior,
          beauti_options = beauti_options
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  if (site_model$kappa_2_param$estimate == TRUE) {
    text <- c(text, paste0("<prior id=\"kappa2Prior.s:", id, "\" ",
                           "name=\"distribution\" x=\"@kappa2.s:", id, "\">"))
    text <- c(
      text,
      beautier::indent(
        beautier::distr_to_xml(
          site_model$kappa_2_prior,
          beauti_options = beauti_options
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  text
}
