test_that("use", {
  check_empty_beautier_folder()

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      beauti_options = create_beauti_options_v2_4()
    )
  )

  created <- rnd_phylo_to_xml_init(inference_model)
  expected <- c(
    "<init id=\"RandomTree.t:test_output_0\" spec=\"beast.evolution.tree.RandomTree\" estimate=\"false\" initial=\"@Tree.t:test_output_0\" taxa=\"@test_output_0\">", # nolint long line indeed
    "    <populationModel id=\"ConstantPopulation0.t:test_output_0\" spec=\"ConstantPopulation\">", # nolint long line indeed
    "        <parameter id=\"randomPopSize.t:test_output_0\" name=\"popSize\">1.0</parameter>", # nolint long line indeed
    "    </populationModel>",
    "</init>"
  )
  expect_equal(created, expected)
})

test_that("use, 2.6", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      beauti_options = create_beauti_options_v2_6()
    )
  )

  created <- rnd_phylo_to_xml_init(inference_model)
  expected <- c(
    "<init id=\"RandomTree.t:test_output_0\" spec=\"beast.evolution.tree.RandomTree\" estimate=\"false\" initial=\"@Tree.t:test_output_0\" taxa=\"@test_output_0\">", # nolint long line indeed
    "    <populationModel id=\"ConstantPopulation0.t:test_output_0\" spec=\"ConstantPopulation\">", # nolint long line indeed
    "        <parameter id=\"randomPopSize.t:test_output_0\" spec=\"parameter.RealParameter\" name=\"popSize\">1.0</parameter>", # nolint long line indeed
    "    </populationModel>",
    "</init>"
  )
  expect_equal(created, expected)

  check_empty_beautier_folder()
})

test_that("abuse", {
  check_empty_beautier_folder()

  expect_error(
    rnd_phylo_to_xml_init(ape::rcoal(3))
  )

  check_empty_beautier_folder()
})
