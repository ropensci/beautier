#' Internal function to creates the XML text for the \code{siteModel} tag
#' of a BEAST2 parameter file.
#'
#' Creates the XML text for the \code{siteModel} tag of
#' a BEAST2 parameter file,
#' which is part of the \code{distribution} node for the
#' \code{treeLikelihood} ID.
#'
#' The \code{siteModel} tag has these elements:
#'
#' \preformatted{
#'   <siteModel[...]>
#'       [parameters]
#'   </siteModel>
#' }
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior"[...]">
#'  #     <distribution id="likelihood" [...]>
#'  #       <siteModel...>
#'  #         [parameters]
#'  #       </siteModel>
#'  #     </distribution>
#'  # </distribution>
#' @export
create_site_model_xml <- function(
  inference_model
) {
  id <- inference_model$site_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL

  # gcc: gamma category count
  gcc <- inference_model$site_model$gamma_site_model$gamma_cat_count
  if (gcc == 0) {
    text <- c(text, paste0("<siteModel id=\"SiteModel.s:",
      id, "\" spec=\"SiteModel\">")
    )
  } else if (gcc == 1) {
    text <- c(text, paste0("<siteModel id=\"SiteModel.s:",
      id, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gcc,
      "\">")
    )
  } else {
    text <- c(text, paste0("<siteModel id=\"SiteModel.s:",
      id, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gcc,
      "\" shape=\"@gammaShape.s:", id, "\">")
    )
  }

  text <- c(text, paste0("    <parameter ",
    "id=\"mutationRate.s:", id,
    "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>"))
  if (gcc < 2) {
    text <- c(text, paste0("    <parameter ",
      "id=\"gammaShape.s:", id,
      "\" estimate=\"false\" name=\"shape\">1.0</parameter>"))
  }

  # proportionInvariant
  text <- c(text, paste0(
    "    <parameter id=\"proportionInvariant.s:",
    id, "\" estimate=\"false\" lower=\"0.0\" ",
    "name=\"proportionInvariant\" upper=\"1.0\">",
    inference_model$site_model$gamma_site_model$prop_invariant,
    "</parameter>"))

  text <- c(text,
    beautier::indent(
      site_model_to_xml_subst_model(inference_model$site_model)
    )
  )

  text <- c(text, "</siteModel>")

  text
}
