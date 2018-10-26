#' Creates the MRCA prior's XML for the tracelog section
#' @inheritParams default_params_doc
#' @param is_first is this the first MRCA prior?
#' @seealso all MRCA priors' tracelog section is created
#'   by \code{\link{mrca_priors_to_xml_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richel J.C. Bilderbeek
#' @noRd
mrca_prior_to_xml_tracelog <- function( # nolint internal function
  mrca_prior,
  has_non_strict_clock_model = FALSE,
  is_first = TRUE
) {
  testit::assert(is_mrca_prior(mrca_prior))
  if (length(mrca_prior) == 1 && is.na(mrca_prior)) return(NULL)

  text <- NULL
  text <- c(text, paste0("<log idref=\"", mrca_prior$name, ".prior\"/>"))

  if (!has_non_strict_clock_model &&
    mrca_prior$is_monophyletic &&
    !is.na(mrca_prior$mrca_distr) &&
    is_first
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
