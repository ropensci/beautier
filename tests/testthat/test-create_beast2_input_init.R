context("create_beast2_input_init")

test_that("use", {

  expect_silent(
    beautier:::create_beast2_input_init(
      ids = "test_output_0"
    )
  )
})

test_that("abuse", {

  expect_error(
    create_beast2_input_init(
      ids = c("a", "b")
    )
  )
})
