test_that("v2.4", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model()
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- c(
    "<state id=\"state\" storeEvery=\"5000\">",
    "    <tree id=\"Tree.t:test_output_0\" name=\"stateNode\">",
    "        <taxonset id=\"TaxonSet.test_output_0\" spec=\"TaxonSet\">",
    "            <alignment idref=\"test_output_0\"/>",
    "        </taxonset>",
    "    </tree>",
    "    <parameter id=\"birthRate.t:test_output_0\" name=\"stateNode\">1.0</parameter>", # nolint long line indeed
    "</state>"
  )
  expect_equal(created, expected)
})

test_that("v2.6", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      beauti_options = create_beauti_options_v2_6()
    )
  )
  expect_silent(create_beast2_input_state(inference_model = inference_model))
})

test_that("birth_death", {

  skip("Obsoleted interface")
  fasta_filename <- beautier::get_beautier_path("test_output_0.fas")
  create_test_inference_model()
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

  skip("Obsoleted interface")

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

  # Obsoleted interface"
  fasta_filename <- beautier::get_beautier_path("test_output_0.fas")
  id <- get_alignment_id(fasta_filename)
  expect_error(
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
    ),
    "'site_models' is deprecated, use 'inference_model' instead"
  )
})

test_that("deprecation", {

  expect_error(
    create_beast2_input_state(
      inference_model = create_test_inference_model(),
      site_models = "something"
    ),
    "'site_models' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    create_beast2_input_state(
      inference_model = create_test_inference_model(),
      clock_models = "something"
    ),
    "'clock_models' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    create_beast2_input_state(
      inference_model = create_test_inference_model(),
      tree_priors = "something"
    ),
    "'tree_priors' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    create_beast2_input_state(
      inference_model = create_test_inference_model(),
      mrca_priors = "something"
    ),
    "'mrca_priors' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    create_beast2_input_state(
      inference_model = create_test_inference_model(),
      tipdates_filename = "something"
    ),
    "'tipdates_filename' is deprecated, use 'inference_model' instead"
  )
})
