#' Creates the XML of an MRCA prior,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the mrca prior as XML text
#' @author Richel J.C. Bilderbeek
mrca_prior_to_xml_operators <- function(
  mrca_prior,
  fixed_crown_age = FALSE
) {
  testit::assert(is_mrca_prior(mrca_prior))
  if (length(mrca_prior) == 1 && is.na(mrca_prior)) return(NULL)
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
