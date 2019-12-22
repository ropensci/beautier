#' Creates the operators section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_operators <- function(
  site_models,
  clock_models,
  tree_priors,
  fixed_crown_ages = rep(FALSE, length(site_models)),
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(beautier::is_one_bool(fixed_crown_ages))
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  testit::assert(length(site_models) == length(fixed_crown_ages))

  text <- NULL


  text <- c(
    text,
    tree_priors_to_xml_operators(
      tree_priors = tree_priors,
      fixed_crown_ages = fixed_crown_ages
    )
  )

  text <- c(text, beautier::site_models_to_xml_operators(site_models))
  text <- c(
    text,
    beautier::clock_models_to_xml_operators(
      clock_models = clock_models,
      mrca_priors = mrca_priors,
      tipdates_filename = tipdates_filename
    )
  )
  text <- beautier::interspace(text)

  beautier::indent(text)
}
