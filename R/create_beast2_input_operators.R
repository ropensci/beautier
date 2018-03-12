#' Creates the operators section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_operators <- function( # nolint internal function
  site_models,
  clock_models,
  tree_priors,
  fixed_crown_ages = rep(FALSE, length(site_models)),
  mrca_priors = NA
) {
  testit::assert(is.logical(fixed_crown_ages))
  testit::assert(are_site_models(site_models))
  testit::assert(are_clock_models(clock_models))
  testit::assert(are_tree_priors(tree_priors))
  testit::assert(are_mrca_priors(mrca_priors))
  testit::assert(length(site_models) == length(fixed_crown_ages))

  text <- NULL


  text <- c(
    text,
    tree_priors_to_xml_operators(
      tree_priors = tree_priors,
      fixed_crown_ages = fixed_crown_ages
    )
  )

  text <- c(text, site_models_to_xml_operators(site_models)) # nolint internal function
  text <- c(text, clock_models_to_xml_operators(clock_models)) # nolint internal function
  text <- c(
    text,
    mrca_priors_to_xml_operators( # nolint internal function
      mrca_priors = mrca_priors,
      has_non_strict_clock_model = get_has_non_strict_clock_model(clock_models) # nolint internal function
    )
  )
  text <- interspace(text) # nolint internal function

  indent(text, n_spaces = 4) # nolint internal function
}
