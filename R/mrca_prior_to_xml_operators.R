#' Creates the XML of an MRCA prior,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the MRCA prior as XML text
#' @author Richel J.C. Bilderbeek
#' @noRd
mrca_prior_to_xml_operators <- function(
  mrca_prior,
  fixed_crown_age = FALSE,
  clock_models
) {
  testit::assert(is_mrca_prior(mrca_prior)) # nolint internal function
  if (length(mrca_prior) == 1 && is.na(mrca_prior)) return(NULL)
  if (!mrca_prior$is_monophyletic && is.na(mrca_prior$mrca_distr)) return(NULL)
  if (all(is.na(mrca_prior$mrca_distr))) return(NULL)
  if (length(clock_models) == 1 &&
      is_rln_clock_model(clock_models[[1]]) # nolint internal function
  ) {
    return(NULL)
  }

  id <- mrca_prior$alignment_id

  text <- NULL
  text <- c(
    text,
    paste0(
      "<operator id=\"StrictClockRateScaler.c:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@clockRate.c:", id, "\" ",
      "scaleFactor=\"0.75\" weight=\"3.0\"/>"
    )
  )
  text <- c(
    text,
    paste0(
      "<operator id=\"strictClockUpDownOperator.c:", id, "\" ",
      "spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">"
    )
  )
  text <- c(text, paste0("    <up idref=\"clockRate.c:", id, "\"/>"))
  text <- c(text, paste0("    <down idref=\"Tree.t:", id, "\"/>"))
  text <- c(text, paste0("</operator>"))
  text
}
