context("create_beast2_input_distr")

test_that("use with one ID", {

  fasta_filename <- beautier::get_beautier_path("test_output_0.fas")
  id <- beautier:::get_alignment_id(fasta_filename)

  xml <- beautier:::create_beast2_input_distr(
    site_models = list(create_jc69_site_model(id = id)),
    clock_models = beautier:::init_clock_models(
      list(create_strict_clock_model(id = NA)),
      fasta_filenames = fasta_filename
    ),
    tree_priors = beautier:::init_tree_priors(
      list(create_yule_tree_prior(id = id)),
      ids = id,
      distr_id = 1
    )
  )
  testthat::expect_true(beautier:::is_xml(xml))
})
