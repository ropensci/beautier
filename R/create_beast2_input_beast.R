#' Creates the beast section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_beast <- function(
  input_filename,
  inference_model = create_inference_model()
) {
  testit::assert(length(input_filename) == 1)
  testit::assert(file.exists(input_filename))

  # Alignment IDs
  ids <- beautier::get_alignment_id(
    input_filename,
    capitalize_first_char_id =
      inference_model$beauti_options$capitalize_first_char_id
  )

  text <- create_beast2_beast_xml( # nolint beautier function
    beast2_version = inference_model$beauti_options$beast2_version,
    required = inference_model$beauti_options$required
  )

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_data(
      input_filenames = input_filename,
      beauti_options = inference_model$beauti_options
    )
  )

  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ") # Exact same spacing as created by BEAUti
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")

  text <- c(text, create_beast2_input_map()) # nolint beautier function call

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_run(
      ids = ids,
      site_models = list(inference_model$site_model),
      clock_models = list(inference_model$clock_model),
      mrca_priors = list(inference_model$mrca_prior),
      tree_priors = list(inference_model$tree_prior),
      mcmc = inference_model$mcmc,
      fixed_crown_ages = FALSE,
      initial_phylogenies = NA,
      tipdates_filename = inference_model$tipdates_filename
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
