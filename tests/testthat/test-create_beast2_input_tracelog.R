context("create_beast2_input_tracelog")

test_that("use", {
  expect_silent(
    beautier:::create_beast2_input_tracelog(
      ids = 1
    )
  )
})
