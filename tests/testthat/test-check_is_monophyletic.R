test_that("use", {
  expect_silent(check_is_monophyletic(TRUE))
  expect_silent(check_is_monophyletic(FALSE))
  expect_error(check_is_monophyletic(""),
    "'is_monophyletic' must be either TRUE or FALSE"
  )
  expect_error(check_is_monophyletic(NA))
  expect_error(check_is_monophyletic(NULL))
  expect_error(check_is_monophyletic(Inf))
  expect_error(check_is_monophyletic(c(TRUE, FALSE)))
})
