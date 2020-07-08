#' Creates the XML text for the \code{siteModel} tag
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
site_model_to_xml_lh_distr <- function(
  inference_model,
  site_model = "deprecated"
) {
  if (site_model != "deprecated") {
    stop("'site_model' is deprecated, use 'inference_model' instead")
  }
  warning(
    "Use of 'site_model_to_xml_lh_distr' is deprecated, ",
    "use 'create_site_model_xml' instead"
  )
  beautier::create_site_model_xml(inference_model)
}
