#' Internal function
#'
#' Converts a site model to XML,
#'   used in the \code{prior} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @examples
#' site_model_to_xml_prior_distr(
#'   site_model = create_jc69_site_model(id = 1),
#'   beauti_options = create_beauti_options()
#' )
#' site_model_to_xml_prior_distr(
#'   site_model = create_hky_site_model(
#'     id = 1,
#'     kappa_prior_distr = create_uniform_distr(id = 2)
#'   ),
#'   beauti_options = create_beauti_options()
#' )
#' site_model_to_xml_prior_distr(
#'   site_model = create_tn93_site_model(
#'     id = 1,
#'     kappa_1_prior_distr = create_uniform_distr(id = 2),
#'     kappa_2_prior_distr = create_uniform_distr(id = 3)
#'   ),
#'   beauti_options = create_beauti_options()
#' )
#' site_model_to_xml_prior_distr(
#'   site_model = create_gtr_site_model(
#'     id = 1,
#'     rate_ac_prior_distr = create_uniform_distr(id = 2),
#'     rate_ag_prior_distr = create_uniform_distr(id = 3),
#'     rate_at_prior_distr = create_uniform_distr(id = 4),
#'     rate_cg_prior_distr = create_uniform_distr(id = 5),
#'     rate_gt_prior_distr = create_uniform_distr(id = 6)
#'   ),
#'   beauti_options = create_beauti_options()
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
site_model_to_xml_prior_distr <- function(
  site_model,
  beauti_options
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (beautier::is_hky_site_model(site_model)) {
    text <- hky_site_model_to_xml_prior_distr(site_model = site_model, beauti_options = beauti_options) # nolint indeed a long line
  } else if (beautier::is_tn93_site_model(site_model)) {
    text <- tn93_site_model_to_xml_prior_distr(site_model = site_model, beauti_options = beauti_options) # nolint indeed a long line
  } else if (beautier::is_gtr_site_model(site_model)) {
    text <- gtr_site_model_to_xml_prior_distr(site_model = site_model, beauti_options = beauti_options) # nolint indeed a long line
  }
  text
}
