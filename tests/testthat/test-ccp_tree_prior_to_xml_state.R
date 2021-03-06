test_that("use", {
  skip("Issue 121")
  skip("https://github.com/ropensci/beautier/issues/121")
  tree_prior = create_ccp_tree_prior(
    pop_size_distr = create_normal_distr(
      initial_value = 100.0,
      lower = 12.0,
      upper = 345.0
    )
  )

  text <- ccp_tree_prior_to_xml_state(
    tree_prior
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
