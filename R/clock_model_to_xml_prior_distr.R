#' Converts a clock model to the \code{prior} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
clock_model_to_xml_prior_distr <- function(
  clock_model,
  is_first = TRUE
) {
  testit::assert(is_clock_model(clock_model))

  text <- NULL
  if (is_rln_clock_model(clock_model)) {

    if (is_first == FALSE) {
      text <- c(text, rln_clock_model_to_xml_mean_rate_prior(clock_model)) # nolint internal function
    }

    id <- clock_model$id
    testit::assert(is_id(id))
    text <- c(text, paste0("<prior ",
      "id=\"ucldStdevPrior.c:", id, "\" name=\"distribution\" ",
      "x=\"@ucldStdev.c:", id, "\">"))
    text <- c(text,
      indent(
        distr_to_xml(
          distr = clock_model$ucldstdev_distr
        ),
        n_spaces = 4
      )
    )
    text <- c(text, paste0("</prior>"))
  } else {
    # Fails for unimplemented clock models
    testit::assert(is_strict_clock_model(clock_model))

    if (is_first == FALSE) {
      id <- clock_model$id
      testit::assert(is_id(id))
      text <- c(text, paste0("<prior id=\"ClockPrior.c:", id, "\" ",
        "name=\"distribution\" x=\"@clockRate.c:", id, "\">"))
      text <- c(text, indent(
        distr_to_xml(clock_model$clock_rate_distr), n_spaces = 4))
      text <- c(text, paste0("</prior>"))
    }
  }
  text
}
