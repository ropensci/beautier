#' Converts one or more MRCA priors to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richel J.C. Bilderbeek
mrca_priors_to_xml_state <- function(
  mrca_priors,
  has_non_strict_clock_model = FALSE
) {
  testit::assert(are_mrca_priors(mrca_priors)) # nolint internal function
  if (length(mrca_priors) == 1 && is.na(mrca_priors)) return(NULL)

  text <- NULL
  is_first <- TRUE
  for (mrca_prior in mrca_priors) {
    text <- c(text,
      mrca_prior_to_xml_state(
        mrca_prior = mrca_prior,
        has_non_strict_clock_model = has_non_strict_clock_model,
        is_first = is_first
      )
    )
    is_first <- FALSE
  }

  text
}
