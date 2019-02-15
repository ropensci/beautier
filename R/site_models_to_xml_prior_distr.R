#' Represent the site models as XML
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' @author Richèl J.C. Bilderbeek
#' @noRd
site_models_to_xml_prior_distr <- function(site_models) {
  text <- NULL
  for (site_model in site_models) {
    text <- c(
      text,
      site_model_to_xml_prior_distr(site_model) # nolint beautier function
    )
  }
  text
}
