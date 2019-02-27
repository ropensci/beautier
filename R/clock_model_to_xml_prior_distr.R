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
#' @noRd
clock_model_to_xml_prior_distr <- function(
  clock_model,
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(is_clock_model(clock_model)) # nolint beautier function

  text <- NULL
  if (is_rln_clock_model(clock_model)) { # nolint beautier function

    if (
      is_mrca_prior_with_distr(mrca_priors[[1]]) # nolint beautier function
    ) {
      text <- c(text, rln_clock_model_to_xml_mean_rate_prior(clock_model)) # nolint beautier function
    }

    id <- clock_model$id
    testit::assert(is_id(id)) # nolint beautier function
    text <- c(text, paste0("<prior ",
      "id=\"ucldStdevPrior.c:", id, "\" name=\"distribution\" ",
      "x=\"@ucldStdev.c:", id, "\">"))
    text <- c(text,
      indent( # nolint beautier function
        distr_to_xml( # nolint beautier function
          distr = clock_model$ucldstdev_distr
        ),
        n_spaces = 4
      )
    )
    text <- c(text, paste0("</prior>"))
  } else {
    # Fails for unimplemented clock models
    testit::assert(is_strict_clock_model(clock_model)) # nolint beautier function

    if (!is_one_na(tipdates_filename)) { # nolint beautier function
      id <- clock_model$id
      testit::assert(is_id(id)) # nolint beautier function
      text <- c(text, paste0("<prior id=\"ClockPrior.c:", id, "\" ",
        "name=\"distribution\" x=\"@clockRate.c:", id, "\">"))
      text <- c(text,
        indent( # nolint beautier function
          distr_to_xml( # nolint beautier function
            clock_model$clock_rate_distr
          ),
          n_spaces = 4
        )
      )
      text <- c(text, paste0("</prior>"))
    }
  }
  text
}
