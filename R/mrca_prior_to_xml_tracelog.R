#' Creates the MRCA prior's XML for the tracelog section
#'
#' Creates the MRCA prior's XML for the tracelog section.
#'
#' \code{
#'   <logger id="tracelog" ...>
#'     # Here
#'   </logger>
#' }
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @seealso all MRCA priors' tracelog section is created
#'   by \code{\link{mrca_priors_to_xml_tracelog}}
#' @author Richèl J.C. Bilderbeek
#' @export
mrca_prior_to_xml_tracelog <- function(
  clock_models,
  mrca_prior,
  tipdates_filename = NA
) {
  testit::assert(beautier::is_mrca_prior(mrca_prior))
  if (length(mrca_prior) == 1 &&
      beautier::is_one_na(mrca_prior) &&
      beautier::is_one_na(tipdates_filename)
  ) {
    return(NULL)
  }

  text <- NULL
  if (!beautier::is_one_na(mrca_prior)) {
    text <- c(text, paste0("<log idref=\"", mrca_prior$name, ".prior\"/>")) # nolint this is no absolute path
  }

  if (
    (
      beautier::is_strict_clock_model(clock_models[[1]]) &&
      beautier::is_mrca_prior_with_distr(mrca_prior)
    ) ||
      !beautier::is_one_na(tipdates_filename)
  ) {
    text <- c(
      text,
      paste0(
        "<log idref=\"clockRate.c:", clock_models[[1]]$id, "\"/>" # nolint this is no absolute path
      )
    )
  }
  text
}
