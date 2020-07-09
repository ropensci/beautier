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
  # Do not be smart yet
  clock_models <- list(inference_model$clock_model)
  mrca_priors <- list(inference_model$mrca_prior)


  text <- NULL
  id <- inference_model$site_model$id
  brm_line <- ""
  text <- c(text, paste0("<distribution id=\"treeLikelihood.",
    id, "\" spec=\"ThreadedTreeLikelihood\" ",
    brm_line,
    "data=\"@", id,
    "\" tree=\"@Tree.t:", id, "\">"))
  text <- c(text,
    beautier::indent(
      beautier::create_site_model_xml(
        inference_model = inference_model
      )
    )
  )
  # branchRateModel
  if (beautier::is_one_na(mrca_priors) ||
      get_has_non_strict_clock_model(clock_models)
  ) {
    text <- c(text,
      beautier::indent(
        beautier::create_branch_rate_model_xml(
          inference_model = inference_model
        )
      )
    )
  }
  # Can be either NA or a list of 1 element
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  testit::assert(length(mrca_priors) >= 1)
  mrca_prior <- NA
  if (!beautier::is_one_na(mrca_priors)) mrca_prior <- mrca_priors[[1]]
  testit::assert(beautier::is_mrca_prior(mrca_prior))
  text <- c(text,
    beautier::indent(
      beautier::mrca_prior_to_xml_lh_distr(
        mrca_prior,
        has_non_strict_clock_model = beautier::get_has_non_strict_clock_model(
          clock_models
        )
      )
    )
  )
  # Close of '<distribution id="treeLikelihood.test_output_0"...'
  text <- c(text, "</distribution>")
  text
}
