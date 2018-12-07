#' Create a BEAST2 input file
#' @inheritParams default_params_doc
#' @examples
#'   # The file created by beautier, a BEAST2 input file
#'   output_filename <- "create_beast2_input_file.xml"
#'
#'   # Birth-Death tree prior, crown age is estimated
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     output_filename
#'   )
#'   testthat::expect_true(file.exists(output_filename))
#' @author Richel J.C. Bilderbeek
#' @seealso See \code{\link{create_site_model}} for examples with
#'   different site models. See \code{\link{create_clock_model}} for examples
#'   with clock models. See \code{\link{create_tree_prior}} for examples with
#'   different tree priors. See \code{\link{create_mcmc}} for examples with
#'   a different MCMC setup.
#' @export
create_beast2_input_file <- function(
  input_filenames,
  output_filename,
  site_models = create_jc69_site_model(id = get_alignment_id(
    input_filenames)
  ),
  clock_models = create_strict_clock_model(
    id = get_alignment_id(input_filenames)
  ),
  tree_priors = create_yule_tree_prior(
    id = get_alignment_id(input_filenames)
  ),
  mrca_priors = NA,
  mcmc = create_mcmc(),
  misc_options = create_misc_options(),
  posterior_crown_age = "deprecated",
  tipdates_filename = NA
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

  # Error handling done by create_beast2_input
  text <- create_beast2_input(
    input_filenames = input_filenames,
    tipdates_filename = tipdates_filename,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    mrca_priors = mrca_priors,
    mcmc = mcmc,
    misc_options = misc_options
  )

  # Write to file
  my_file <- file(output_filename)
  writeLines(text, my_file)
  close(my_file)
}
