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
      beautier::phylo_to_xml_state(
        id = id,
        tipdates_filename = tipdates_filename
      )
    )
  }

  text <- c(text, beautier::site_models_to_xml_state(site_models))
  text <- c(
    text,
    beautier::clock_models_to_xml_state(
      clock_models = clock_models,
      mrca_priors = mrca_priors,
      has_tip_dating = has_tip_dating
    )
  )
  text <- c(text, beautier::tree_priors_to_xml_state(tree_priors))
  text <- c(
    text,
    beautier::mrca_priors_to_xml_state(
      mrca_priors,
      has_non_strict_clock_model = beautier::get_has_non_strict_clock_model(
        clock_models
      )
    )
  )

  text <- beautier::indent(text)
  text <- c("<state id=\"state\" storeEvery=\"5000\">", text)
  text <- c(text, "</state>")
  text
}
