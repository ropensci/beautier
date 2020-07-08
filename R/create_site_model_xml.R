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
#'
#'       [parameters]
#'
#'       <substModel[...]>
#'         [...]
#'       </substModel>
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

  # gcc: gamma category count
  gcc <- inference_model$site_model$gamma_site_model$gamma_cat_count

  site_model_begin_tag <- paste0(
    "<siteModel id=\"SiteModel.s:", id, "\" spec=\"SiteModel\""
  )
  if (gcc >= 1) {
    site_model_begin_tag <- paste0(
      site_model_begin_tag,
      " gammaCategoryCount=\"", gcc, "\""
    )
  }
  if (gcc >= 2) {
    site_model_begin_tag <- paste0(
      site_model_begin_tag,
      " shape=\"@gammaShape.s:", id, "\""
    )
  }
  site_model_begin_tag <- paste0(site_model_begin_tag, ">")


  mutation_rate_parameter <- paste0(
    "<parameter ",
    "id=\"mutationRate.s:", id,
    "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>"
  )

  gamma_shape_parameter <- NULL
  if (gcc < 2) {
    gamma_shape_parameter <- paste0(
      "<parameter ",
      "id=\"gammaShape.s:", id,
      "\" estimate=\"false\" name=\"shape\">1.0</parameter>"
    )
  }
  proportion_invariant_parameter <- paste0(
    "<parameter id=\"proportionInvariant.s:",
    id, "\" estimate=\"false\" lower=\"0.0\" ",
    "name=\"proportionInvariant\" upper=\"1.0\">",
    inference_model$site_model$gamma_site_model$prop_invariant,
    "</parameter>"
  )
  subst_model_xml <- beautier::create_subst_model_xml(inference_model)
  site_model_end_tag <- "</siteModel>"

  # Layout of the text
  text <- c(
    site_model_begin_tag,
    paste0("    ", mutation_rate_parameter)
  )
  if (!is.null(gamma_shape_parameter)) {
    text <- c(text, paste0("    ", gamma_shape_parameter))
  }
  text <- c(
    text,
    paste0("    ", proportion_invariant_parameter),
    beautier::indent(subst_model_xml),
    site_model_end_tag
  )
}
