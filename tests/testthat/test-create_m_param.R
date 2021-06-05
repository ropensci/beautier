test_that("two names are the name", {
  # Two versions: English and searchable
  expect_identical(create_m_param(), create_param_m())
})

test_that("use, valid function arguments", {
  expect_silent(create_m_param(id = 1))
  expect_silent(create_m_param(estimate = TRUE))
  expect_silent(create_m_param(value = 12.34))
  expect_silent(create_m_param(lower = 0.5))
  expect_silent(create_m_param(upper = 314.15))
})
