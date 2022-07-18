test_that("use", {
  expect_silent(is_on_appveyor())
  expect_silent(is_on_github_actions())
  expect_silent(is_on_travis())
})
