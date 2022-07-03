#' Internal function
#'
#' Internal function to creates the MRCA prior's XML for the tracelog section.
#'
#' \code{
#'   <logger id="tracelog" ...>
#'     # Here
#'   </logger>
#' }
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
mrca_prior_to_xml_tracelog <- function(
  inference_model
) {
  # Do not be smart yet
  mrca_prior <- inference_model$mrca_prior

  testit::assert(beautier::is_mrca_prior(mrca_prior))
  if (!beautier::has_mrca_prior(inference_model) &&
      !beautier::has_tip_dating(inference_model)
  ) {
    return(NULL)
  }

  text <- NULL
  if (!beautier::is_one_na(mrca_prior)) {
    text <- c(text, paste0("<log idref=\"", mrca_prior$name, ".prior\"/>")) # nolint this is no absolute path
  }

  if (
    (
      beautier::is_strict_clock_model(inference_model$clock_model) &&
      beautier::has_mrca_prior_with_distr(inference_model)
    ) ||
      (
        beautier::is_strict_clock_model(inference_model$clock_model) &&
        beautier::has_tip_dating(inference_model)
      )
  ) {
    text <- c(
      text,
      paste0(
        "<log idref=\"clockRate.c:", inference_model$clock_model$id, "\"/>" # nolint this is no absolute path
      )
    )
  }
  text
}
