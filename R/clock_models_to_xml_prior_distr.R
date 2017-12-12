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
#' @author Richel J.C. Bilderbeek
clock_models_to_xml_prior_distr <- function(clock_models) { # nolint internal function name
  text <- NULL

  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    text <- c(
      text,
      clock_model_to_xml_prior_distr(clock_model, i == 1) # nolint internal function
    )
  }
  text
}