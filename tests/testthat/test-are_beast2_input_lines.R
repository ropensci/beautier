context("are_beast2_input_lines")

test_that("use", {

  testthat::expect_true(
    beautier:::are_beast2_input_lines(
      readLines(beautier:::get_path("2_4.xml"))
    )
  )

  testthat::expect_true(
    beautier:::are_beast2_input_lines(
      readLines(beautier:::get_path("2_4.xml")),
      method = "deep"
    )
  )

  testthat::expect_true(
    beautier:::are_beast2_input_lines(
      readLines(beautier:::get_path("2_4.xml")),
      method = "fast"
    )
  )

})

test_that("abuse", {

  testthat::expect_error(
    beautier:::are_beast2_input_lines(
      readLines(beautier:::get_path("2_4.xml")),
      method = "nonsense"
    ),
    "'method' must be \"deep\" or \"fast\""
  )

})
