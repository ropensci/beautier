#' Converts a clock model to the \code{prior} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' @export
clock_model_to_xml_prior_distr <- function(
  clock_model,
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(beautier::is_clock_model(clock_model))

  text <- NULL
  if (beautier::is_rln_clock_model(clock_model)) {

    if (
      beautier::is_mrca_prior_with_distr(mrca_priors[[1]])
    ) {
      text <- c(
        text,
        beautier::rln_clock_model_to_xml_mean_rate_prior(clock_model)
      )
    }

    id <- clock_model$id
    testit::assert(beautier::is_id(id))
    text <- c(text, paste0("<prior ",
      "id=\"ucldStdevPrior.c:", id, "\" name=\"distribution\" ",
      "x=\"@ucldStdev.c:", id, "\">"))
    text <- c(text,
      beautier::indent(
        beautier::distr_to_xml(
          distr = clock_model$ucldstdev_distr
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  } else {
    # Fails for unimplemented clock models
    testit::assert(beautier::is_strict_clock_model(clock_model))

    if (!beautier::is_one_na(tipdates_filename)) {
      id <- clock_model$id
      testit::assert(beautier::is_id(id))
      text <- c(text, paste0("<prior id=\"ClockPrior.c:", id, "\" ",
        "name=\"distribution\" x=\"@clockRate.c:", id, "\">"))
      text <- c(text,
        beautier::indent(
          beautier::distr_to_xml(
            clock_model$clock_rate_distr
          )
        )
      )
      text <- c(text, paste0("</prior>"))
    }
  }
  text
}
