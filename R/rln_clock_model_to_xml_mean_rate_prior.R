#' Internal function
#'
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
rln_clock_model_to_xml_mean_rate_prior <- function( # nolint indeed a long internal function name
  rln_clock_model,
  beauti_options
) { # nolint indeed long function name

  beautier::check_true(beautier::is_rln_clock_model(rln_clock_model))
  id <- rln_clock_model$id
  beautier::check_true(beautier::is_id(id))

  text <- NULL

  text <- c(
    text,
    paste0(
      "<prior id=\"MeanRatePrior.c:", id, "\" ",
      "name=\"distribution\" x=\"@ucldMean.c:", id, "\">"
    )
  )
  text <- c(text,
    beautier::indent(
      beautier::distr_to_xml(
        distr = rln_clock_model$mean_rate_prior_distr,
        beauti_options = beauti_options
      )
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}
