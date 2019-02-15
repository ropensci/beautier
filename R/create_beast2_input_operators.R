#' Creates the operators section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_operators <- function( # nolint beautier function
  site_models,
  clock_models,
  tree_priors,
  fixed_crown_ages = rep(FALSE, length(site_models)),
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(is.logical(fixed_crown_ages))
  testit::assert(are_site_models(site_models)) # nolint beautier function
  testit::assert(are_clock_models(clock_models)) # nolint beautier function
  testit::assert(are_tree_priors(tree_priors)) # nolint beautier function
  testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function
  testit::assert(length(site_models) == length(fixed_crown_ages))

  text <- NULL


  text <- c(
    text,
    tree_priors_to_xml_operators(
      tree_priors = tree_priors,
      fixed_crown_ages = fixed_crown_ages
    )
  )

  text <- c(text, site_models_to_xml_operators(site_models)) # nolint beautier function
  text <- c(
    text,
    clock_models_to_xml_operators( # nolint beautier function
      clock_models = clock_models,
      mrca_priors = mrca_priors,
      tipdates_filename = tipdates_filename
    )
  )
  text <- interspace(text) # nolint beautier function

  indent(text, n_spaces = 4) # nolint beautier function
}
