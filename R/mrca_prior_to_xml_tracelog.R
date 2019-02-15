#' Creates the MRCA prior's XML for the tracelog section
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @seealso all MRCA priors' tracelog section is created
#'   by \code{\link{mrca_priors_to_xml_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richèl J.C. Bilderbeek
#' @noRd
mrca_prior_to_xml_tracelog <- function( # nolint beautier function
  clock_models,
  mrca_prior,
  tipdates_filename = NA
) {
  testit::assert(is_mrca_prior(mrca_prior)) # nolint beautier function
  if (length(mrca_prior) == 1 &&
      is.na(mrca_prior) &&
      is.na(tipdates_filename)) {
    return(NULL)
  }

  text <- NULL
  if (!is_one_na(mrca_prior)) {
    text <- c(text, paste0("<log idref=\"", mrca_prior$name, ".prior\"/>"))
  }

  if (
    (
      is_strict_clock_model(clock_models[[1]]) &&
      is_mrca_prior_with_distr(mrca_prior)
    ) ||
      !is.na(tipdates_filename)
  ) {
    text <- c(
      text,
      paste0(
        "<log idref=\"clockRate.c:", clock_models[[1]]$id, "\"/>"
      )
    )
  }
  text
}
