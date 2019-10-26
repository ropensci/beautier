#' Used by \code{\link{clock_models_to_xml_prior_distr}}
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
rln_clock_model_to_xml_mean_rate_prior <- function(rln_clock_model) { # nolint beautier function

  testit::assert(beautier::is_rln_clock_model(rln_clock_model))
  id <- rln_clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL

  text <- c(text, paste0("<prior id=\"MeanRatePrior.c:", id, "\" ",
    "name=\"distribution\" x=\"@ucldMean.c:", id, "\">"))
  text <- c(text,
    beautier::indent(
      distr_to_xml( # nolint beautier function
        distr = rln_clock_model$mean_rate_prior_distr
      )
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}
