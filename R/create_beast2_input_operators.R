#' Creates the operators section of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_operators <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_models,
  clock_models,
  tree_priors,
  fixed_crown_age
) {
  testit::assert(is.logical(fixed_crown_age))
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))

  text <- NULL

  for (tree_prior in tree_priors) {
    text <- c(
      text,
      interspace(
        indent(
          tree_prior_to_xml_operators(
            tree_prior = tree_prior,
            fixed_crown_age = fixed_crown_age
          ),
          n_spaces = 4
        )
      )
    )
  }

  for (site_model in site_models) {
    text <- c(
      text,
      interspace(
        indent(
          site_model_to_xml_operators(site_model),
          n_spaces = 4
        )
      )
    )
  }


  for (clock_model in clock_models) {
    text <- c(
      text,
      interspace(
        indent(
          clock_model_to_xml_operators(clock_model),
          n_spaces = 4
        )
      )
    )
  }

  text
}
