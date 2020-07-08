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
#'
#' The \code{parameter} section is created by
#' \link{create_site_model_parameters_xml}
#' The \code{substModel} section is created by \link{create_subst_model_xml}
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


  site_model_parameters <- beautier::create_site_model_parameters_xml(
    inference_model
  )
  subst_model_xml <- beautier::create_subst_model_xml(inference_model)
  site_model_end_tag <- "</siteModel>"

  # Layout of the text
  c(
    site_model_begin_tag,
    beautier::indent(site_model_parameters),
    beautier::indent(subst_model_xml),
    site_model_end_tag
  )
}
