rln_clock_model_to_xml_mean_rate_prior <- function(rln_clock_model) {

  testit::assert(beautier::is_rln_clock_model(rln_clock_model))
  id <- rln_clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL

  text <- c(text, paste0("<prior id=\"MeanRatePrior.c:", id, "\" ",
    "name=\"distribution\" x=\"@ucldMean.c:", id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        distr = rln_clock_model$mean_rate_prior_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}
