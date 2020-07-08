test_that("use", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model()
  )

  created <- create_beast2_input_init(
    inference_model = inference_model
  )
  expected <- c(
    "",
    "    <init id=\"RandomTree.t:test_output_0\" spec=\"beast.evolution.tree.RandomTree\" estimate=\"false\" initial=\"@Tree.t:test_output_0\" taxa=\"@test_output_0\">", # nolint indeed a long line
    "        <populationModel id=\"ConstantPopulation0.t:test_output_0\" spec=\"ConstantPopulation\">", # nolint indeed a long line
    "            <parameter id=\"randomPopSize.t:test_output_0\" name=\"popSize\">1.0</parameter>", # nolint indeed a long line
    "        </populationModel>",
    "    </init>"
  )
  expect_equal(created, expected)
})

test_that("abuse", {

  expect_error(
    create_beast2_input_init(
      id = c("a", "b")
    )
  )
})

test_that("deprecation", {

  expect_error(
    create_beast2_input_init(
      id = "something",
      inference_model = "irrelevant"
    ),
    "'id' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    create_beast2_input_init(
      ids = "something",
      inference_model = "irrelevant"
    ),
    "'ids' is deprecated, use 'inference_model' instead"
  )
})
