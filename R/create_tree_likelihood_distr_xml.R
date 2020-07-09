#' Creates the XML text for the \code{distribution} tag
#' with the \code{treeLikelihood} ID,
#' of a BEAST2 parameter file.
#'
#' Creates the XML text for the \code{distribution} tag
#' with the \code{treeLikelihood} ID,
#' of a BEAST2 parameter file,
#' in an unindented form
#'
#' The \code{distribution} tag (with ID equals \code{treeLikelihood})
#' has these elements:
#'
#' \preformatted{
#'    <distribution id="treeLikelihood"[...]>
#'       <siteModel[...]>
#'         [...]
#'       </siteModel>
#'       <branchRateModel[...]>
#'         [...]
#'       </branchRateModel>
#'    </distribution>
#' }
#'
#' The \code{siteModel} section
#' is created by \link{create_site_model_xml}.
#' The \code{branchRateModel} section
#' is created by \link{create_branch_rate_model_xml}.
#'
#' Zooming out:
#'
#' \preformatted{
#'   <beast[...]>
#'     <run[...]>
#'       <distribution id="posterior"[...]>
#'         <distribution id="likelihood"[...]>
#'           [this section]
#'         </distribution>
#'       </distribution>
#'     </run>
#'   </beast>
#' }
#'
#' @inheritParams default_params_doc
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richèl J.C. Bilderbeek
#' @seealso this function is called by \code{create_beast2_input_distr},
#'   together with \code{create_beast2_input_distr_prior}
#' @export
create_tree_likelihood_distr_xml <- function(# nolint long function name indeed
  inference_model
) {
  text <- NULL
  id <- inference_model$site_model$id
  brm_line <- ""
  text <- c(text, paste0("<distribution id=\"treeLikelihood.",
    id, "\" spec=\"ThreadedTreeLikelihood\" ",
    brm_line,
    "data=\"@", id,
    "\" tree=\"@Tree.t:", id, "\">"))

  # Create the '<siteModel' XML section
  text <- c(text,
    beautier::indent(
      beautier::create_site_model_xml(
        inference_model = inference_model
      )
    )
  )
  # Create the '<branchRateModel' XML section
  text <- c(text,
    beautier::indent(
      beautier::create_branch_rate_model_xml(
        inference_model = inference_model
      )
    )
  )

  # Close of '<distribution id="treeLikelihood.test_output_0"...'
  text <- c(text, "</distribution>")
  text
}
