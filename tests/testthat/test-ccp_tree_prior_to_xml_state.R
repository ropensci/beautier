test_that("minimal use, v2.4", {
  inference_model <- create_inference_model(
    tree_prior = create_ccp_tree_prior(
      id = "anthus_nd2_sub",
      pop_size_distr = create_normal_distr(
        id = 123,
        value = 3.14
      )
    )
  )

  text <- ccp_tree_prior_to_xml_state(
    inference_model
  )

  text
  # This is the text to obtain:
  expected <- paste0(
    "<parameter id=\"popSize.t:anthus_nd2_sub\" name=\"stateNode\">3.14",
    "</parameter>"
  )
  expect_equal(text, expected)
})

test_that("minimal use, v2.6", {
  inference_model <- create_inference_model(
    tree_prior = create_ccp_tree_prior(
      id = "anthus_nd2_sub",
      pop_size_distr = create_normal_distr(
        id = 123,
        value = "0.3"
      )
    ),
    beauti_options = create_beauti_options_v2_6()
  )

  text <- ccp_tree_prior_to_xml_state(
    inference_model
  )

  text
  # This is the text to obtain:
  expected <- paste0(
    "<parameter id=\"popSize.t:anthus_nd2_sub\" ",
      "spec=\"parameter.RealParameter\" name=\"stateNode\">",
      "0.3",
    "</parameter>"
  )
  expect_equal(text, expected)
})

test_that("use", {
  inference_model <- create_inference_model(
    tree_prior = create_ccp_tree_prior(
      id = "anthus_nd2_sub",
      pop_size_distr = create_normal_distr(
        id = 123,
        value = "100.0",
        lower = "12.0",
        upper = "345.0"
      )
    ),
    beauti_options = create_beauti_options_v2_6()
  )

  text <- ccp_tree_prior_to_xml_state(
    inference_model
  )

  text
  # This is the text to obtain:
  expected <- paste0(
    "<parameter id=\"popSize.t:anthus_nd2_sub\" ",
      "spec=\"parameter.RealParameter\" lower=\"12.0\" name=\"stateNode\" ",
      "upper=\"345.0\">",
      "100.0",
    "</parameter>"
  )
  expect_equal(text, expected)
})
