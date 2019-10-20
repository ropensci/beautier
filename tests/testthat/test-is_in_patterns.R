context("is_in_patterns")

test_that("use", {

  patterns <- c(
    "'clock_models' must be a valid clock model",
    "'tree_priors' must be a valid tree prior",
    "'site_models' must be a valid site model",
    "'mrca_priors' must be NA or a valid mrca object"
  )
  expect_true(
    is_in_patterns(
      line = "'clock_models' must be a valid clock model",
      patterns = patterns
    )
  )
  expect_false(
    is_in_patterns(
      line = "absent",
      patterns = patterns
    )
  )


})
