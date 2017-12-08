#' Converts a site model to XML,
#'   used in the \code{siteModel} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #     </distribution>
#'  #     <distribution id="likelihood" spec="util.CompoundDistribution" useThreads="true">
#'  #       HERE, where the ID of the distribution is 'likelihood'
#'  #     </distribution>
#'  # </distribution>
site_model_to_xml_lh_distr <- function(
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL

  gamma_category_count <- beautier::get_gamma_cat_count(
    beautier::get_gamma_site_model(site_model))
  if (gamma_category_count == 0) {
    text <- c(text, paste0("<siteModel id=\"SiteModel.s:",
      id, "\" spec=\"SiteModel\">")
    )
  } else if (gamma_category_count == 1) {
    text <- c(text, paste0("<siteModel id=\"SiteModel.s:",
      id, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gamma_category_count,
      "\">")
    )
  } else {
    text <- c(text, paste0("<siteModel id=\"SiteModel.s:",
      id, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gamma_category_count,
      "\" shape=\"@gammaShape.s:", id, "\">")
    )
  }

  text <- c(text, paste0("    <parameter ",
    "id=\"mutationRate.s:", id,
    "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>"))
  if (gamma_category_count < 2) {
    text <- c(text, paste0("    <parameter ",
      "id=\"gammaShape.s:", id,
      "\" estimate=\"false\" name=\"shape\">1.0</parameter>"))
  }

  # proportionInvariant
  text <- c(text, paste0(
    "    <parameter id=\"proportionInvariant.s:",
    id, "\" estimate=\"false\" lower=\"0.0\" ",
    "name=\"proportionInvariant\" upper=\"1.0\">",
    beautier::get_prop_invariant(
      beautier::get_gamma_site_model(site_model)
    ),
    "</parameter>"))

  text <- c(text,
    beautier::indent(
      site_model_to_xml_subst_model(site_model),
      n_spaces = 4
    )
  )

  text <- c(text, "</siteModel>")

  text
}
