#' Represent the clock models as XML
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richel J.C. Bilderbeek
#' @noRd
clock_models_to_xml_prior_distr <- function( # nolint internal function
  clock_models,
  mrca_priors = NA,
  tipdates_filename = NA
) {
  clock_models <- get_unlinked_clock_models(clock_models)  # nolint internal function
  text <- NULL

  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    text <- c(
      text,
      clock_model_to_xml_prior_distr( # nolint internal function
        clock_model = clock_model,
        mrca_priors = mrca_priors,
        tipdates_filename = tipdates_filename
      )
    )
  }
  text
}
