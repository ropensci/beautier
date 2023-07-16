test_that("use", {
  id <- 314
  expected <- paste0(
    "<parameter id=\"bPopSizes.t:", id, "\" ",
    "dimension=\"5\" lower=\"0.0\" name=\"stateNode\" ",
    "upper=\"380000.0\">380.0</parameter>"
  )
  expect_equal(b_pop_sizes_param_to_xml(create_b_pop_sizes_param(id = id)), expected)
})
