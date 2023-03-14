#' Internal function to create the \code{branchRateModel} section
#' of the XML as text.
#'
#' Creates the \code{branchRateModel} section
#' of the XML as text.
#'
#' The \code{distribution} tag (with ID equals \code{treeLikelihood})
#' has these elements:
#'
#' \preformatted{
#'   <branchRateModel[...]>
#'     [...]
#'   </branchRateModel>
#' }
#'
#' When there is a strict clock,
#'   \link{create_strict_clock_branch_rate_model_xml} is called.
#' When there is an RLN clock,
#'   \link{create_rln_clock_branch_rate_model_xml} is called.
#'
#' Zooming out:
#'
#' \preformatted{
#'   <beast[...]>
#'     <run[...]>
#'       <distribution id="posterior"[...]>
#'         <distribution id="likelihood"[...]>
#'           <distribution id="treeLikelihood"[...]>
#'              [...]
#'
#'              [this section]
#'           </distribution>
#'         </distribution>
#'       </distribution>
#'     </run>
#'   </beast>
#' }
#'
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
create_branch_rate_model_xml <- function(# nolint long function name, which is fine for a long function
  inference_model
) {
  if (beautier::is_strict_clock_model(inference_model$clock_model)) {
    return(beautier::create_strict_clock_branch_rate_model_xml(inference_model))
  } else {
    testthat::expect_true(
      beautier::is_rln_clock_model(inference_model$clock_model)
    )
    return(beautier::create_rln_clock_branch_rate_model_xml(inference_model))
  }
}
