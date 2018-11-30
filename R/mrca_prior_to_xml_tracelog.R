#' Creates the MRCA prior's XML for the tracelog section
#' @inheritParams default_params_doc
#' @seealso all MRCA priors' tracelog section is created
#'   by \code{\link{mrca_priors_to_xml_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richel J.C. Bilderbeek
#' @noRd
mrca_prior_to_xml_tracelog <- function( # nolint internal function
  clock_models,
  mrca_prior
) {
  #has_non_strict_clock_model <- is_rln_clock_model(clock_models[[1]]) # nolint internal function

  testit::assert(is_mrca_prior(mrca_prior)) # nolint internal function
  if (length(mrca_prior) == 1 && is.na(mrca_prior)) {
    return(NULL)
  }

  text <- NULL
  text <- c(text, paste0("<log idref=\"", mrca_prior$name, ".prior\"/>"))

  if (!is_rln_clock_model(clock_models[[1]]) &&
    !is.na(mrca_prior$mrca_distr)
  ) {
    text <- c(
      text,
      paste0(
        "<log idref=\"clockRate.c:", mrca_prior$alignment_id, "\"/>"
      )
    )
  }
  text
}
