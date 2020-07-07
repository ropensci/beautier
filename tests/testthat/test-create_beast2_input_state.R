test_that("birth_death", {

  fasta_filename <- beautier::get_beautier_path("test_output_0.fas")
  id <- get_alignment_id(fasta_filename)
  expect_silent(
    create_beast2_input_state(
      site_models = list(create_jc69_site_model(id = id)),
      clock_models = init_clock_models(
        clock_models = list(create_strict_clock_model()),
        fasta_filenames = fasta_filename,
        distr_id = 0
      ),
      tree_priors = init_tree_priors(
        list(create_yule_tree_prior()), ids = id, distr_id = 1
      )
    )
  )

  expect_silent(
    create_beast2_input_state(
      site_models = list(create_gtr_site_model(id = id)),
      clock_models = init_clock_models(
        clock_models = list(create_strict_clock_model()),
        fasta_filenames = fasta_filename,
        distr_id = 0
      ),
      tree_priors = init_tree_priors(
        list(create_yule_tree_prior()), ids = id, distr_id = 1
      )
    )
  )
})


test_that("use without initial phylogeny", {

  id <- "test_output_0"
  expect_silent(
    create_beast2_input_state(
      site_models = list(create_jc69_site_model(id = id)),
      clock_models = list(create_strict_clock_model(id = id)),
      tree_priors = init_tree_priors(
        list(create_yule_tree_prior()),
        ids = id
      )
    )
  )
})

test_that("v2.4", {

  fasta_filename <- beautier::get_beautier_path("test_output_0.fas")
  id <- get_alignment_id(fasta_filename)
  created <- create_beast2_input_state(
    site_models = list(create_jc69_site_model(id = id)),
    clock_models = init_clock_models(
      clock_models = list(create_strict_clock_model()),
      fasta_filenames = fasta_filename,
      distr_id = 0
    ),
    tree_priors = init_tree_priors(
      list(create_yule_tree_prior()), ids = id, distr_id = 1
    )
  )
  expected <- c(
    "<state id=\"state\" storeEvery=\"5000\">",
    "    <tree id=\"Tree.t:test_output_0\" name=\"stateNode\">",
    "        <taxonset id=\"TaxonSet.test_output_0\" spec=\"TaxonSet\">",
    "            <alignment idref=\"test_output_0\"/>",
    "        </taxonset>",
    "    </tree>",
    "    <parameter id=\"birthRate.t:test_output_0\" name=\"stateNode\">1.0</parameter>",
    "</state>"
  )
  expect_equal(created, expected)
})
