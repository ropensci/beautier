context("is_in_patterns")

test_that("use", {

  patterns <- c(
    "'clock_models' must be a valid clock model",
    "'tree_priors' must be a valid tree prior",
    "'site_models' must be a valid site model",
    "'mrca_priors' must be NA or a valid mrca object",
    "'posterior_crown_age' must be either NA or a non-zero postive value"
  )
  testthat::expect_true(
    beautier:::is_in_patterns(
      line = "'clock_models' must be a valid clock model",
      patterns = patterns
    )
  )
  testthat::expect_false(
    beautier:::is_in_patterns(
      line = "absent",
      patterns = patterns
    )
  )


})
