#' Internal function
#'
#' Converts an HKY site model to XML,
#'   used in the \code{prior} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @examples
#' hky_site_model_to_xml_prior_distr(
#'   site_model = create_hky_site_model(
#'     id = 1,
#'     kappa_prior_distr = create_uniform_distr(id = 2)
#'   ),
#'   beauti_options = create_beauti_options()
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
hky_site_model_to_xml_prior_distr <- function( # nolint indeed a long internal function name
  site_model,
  beauti_options
) {
  check_true(is_hky_site_model(site_model))
  id <- site_model$id
  check_true(is_id(id))

  text <- NULL
  text <- c(text, paste0("<prior ",
                         "id=\"KappaPrior.s:", id, "\" ",
                         "name=\"distribution\" x=\"@kappa.s:", id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        site_model$kappa_prior,
        beauti_options = beauti_options
      )
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}
