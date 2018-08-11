context("is_index_of_first_shared_clock_model")

test_that("one strict clock model is never a shared clock model", {

  clock_model <- list(create_strict_clock_model(id = "a"))
  testit::assert(length(clock_model) == 1)
  # It is not shared
  expect_false(beautier:::is_index_of_first_shared_clock_model(1, clock_model))
})

test_that("one RLN clock model is never a shared clock model", {

  clock_model <- list(create_rln_clock_model(id = "a"))
  testit::assert(length(clock_model) == 1)
  # It is not shared
  expect_false(beautier:::is_index_of_first_shared_clock_model(1, clock_model))
})

test_that("use, two", {

  a <- create_strict_clock_model(id = "a")
  b <- create_rln_clock_model(id = "b")
  aa <- list(a, a) # shared
  ab <- list(a, b)

  expect_true(beautier:::is_index_of_first_shared_clock_model(1, aa))
  expect_false(beautier:::is_index_of_first_shared_clock_model(2, aa))
  expect_false(beautier:::is_index_of_first_shared_clock_model(1, ab))
})

test_that("use, three", {

  a <- create_strict_clock_model(id = "a")
  clock_models <- list(a, a, a) # shared

  expect_true(beautier:::is_index_of_first_shared_clock_model(1, clock_models))
  expect_false(beautier:::is_index_of_first_shared_clock_model(2, clock_models))
  expect_false(beautier:::is_index_of_first_shared_clock_model(3, clock_models))
})
