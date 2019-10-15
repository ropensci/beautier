#' Create a BEAST2 XML input text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @seealso
#'   Use \link{create_beast2_input_from_model} to create the BEAST2 XML
#'   input text from an inference model
#'   Use \link{create_beast2_input_file} to also save it to file.
#' @examples
#'   text <- create_beast2_input(
#'     input_filename = get_fasta_filename()
#'   )
#'   testit::assert(substr(text[1], 1, 5) == "<?xml")
#'   text[1]
#'   testit::assert(tail(text, n = 1) == "</beast>")
#' @seealso \code{\link{create_beast2_input_file}} shows more examples
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input <- function(
  input_filename,
  tipdates_filename = NA,
  site_model = create_jc69_site_model(),
  clock_model = create_strict_clock_model(),
  tree_prior = create_yule_tree_prior(),
  mrca_prior = NA,
  mcmc = create_mcmc(),
  beauti_options = create_beauti_options(),
  input_filenames = "deprecated",
  site_models = "deprecated",
  clock_models = "deprecated",
  tree_priors = "deprecated",
  mrca_priors = "deprecated",
  posterior_crown_age = "deprecated"
) {
  # Check for deprecated argument names
  calls <- names(sapply(match.call(), deparse))[-1]
  if (any("posterior_crown_age" %in% calls)) {
    stop(
      "'posterior_crown_age' is deprecated. \n",
      "Tip: use an MRCA prior ",
      "with a narrow distribution around the crown age instead. \n",
      "See 'create_mrca_prior' or the example below:\n",
      "\n",
      "fasta_filename <- get_beautier_path(\"anthus_aco.fas\")\n",
      "crown_age <- 15\n",
      "\n",
      "mrca_prior <- create_mrca_prior(\n",
      "  alignment_id = get_alignment_id(fasta_filename = fasta_filename),\n",
      "  taxa_names = get_taxa_names(filename = fasta_filename),\n",
      "  mrca_distr = create_normal_distr(\n",
      "    mean = crown_age,\n",
      "    sigma = 0.0001\n",
      "  ),\n",
      "  is_monophyletic = TRUE\n",
      ")\n",
      "\n",
      "create_beast2_input(\n",
      "  input_filename = fasta_filename,\n",
      "  mrca_prior = mrca_prior\n",
      ")\n"
    )
  }
  if (any("input_filenames" %in% calls)) {
    stop("'input_filenames' is deprecated, use 'input_filename' instead.")
  }
  if (any("site_models" %in% calls)) {
    stop("'site_models' is deprecated, use 'site_model' instead.")
  }
  if (any("clock_models" %in% calls)) {
    stop("'clock_models' is deprecated, use 'clock_model' instead.")
  }
  if (any("tree_priors" %in% calls)) {
    stop("'tree_priors' is deprecated, use 'tree_prior' instead.")
  }
  if (any("mrca_priors" %in% calls)) {
    stop("'mrca_priors' is deprecated, use 'mrca_prior' instead.")
  }
  inference_model <- create_inference_model(
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mrca_prior = mrca_prior,
    mcmc = mcmc,
    beauti_options = beauti_options,
    tipdates_filename = tipdates_filename
  )
  create_beast2_input_from_model(
    input_filename = input_filename,
    inference_model = inference_model
  )
}
