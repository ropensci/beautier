#' Creates the state section of a BEAST2 XML parameter file
#' @return lines of XML text
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_state <- function(
  site_models,
  clock_models,
  tree_priors,
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(beautier::are_tree_priors(tree_priors))
  has_tip_dating <- !beautier::is_one_na(tipdates_filename)

  text <- NULL
  for (i in seq_along(tree_priors)) {
    tree_prior <- tree_priors[[i]]
    id <- tree_prior$id
    text <- c(
      text,
      phylo_to_xml_state( # nolint beautier function
        id = id,
        tipdates_filename = tipdates_filename
      )
    )
  }

  text <- c(text, site_models_to_xml_state(site_models)) # nolint beautier function
  text <- c(
    text,
    clock_models_to_xml_state(# nolint beautier function
      clock_models = clock_models,
      mrca_priors = mrca_priors,
      has_tip_dating = has_tip_dating
    )
  )
  text <- c(text, tree_priors_to_xml_state(tree_priors)) # nolint beautier function
  text <- c(
    text,
    mrca_priors_to_xml_state( # nolint beautier function
      mrca_priors,
      has_non_strict_clock_model = get_has_non_strict_clock_model(clock_models) # nolint beautier function
    )
  )

  text <- beautier::indent(text)
  text <- c("<state id=\"state\" storeEvery=\"5000\">", text)
  text <- c(text, "</state>")
  text
}
