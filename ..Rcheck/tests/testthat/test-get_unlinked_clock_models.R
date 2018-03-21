context("get_unlinked_clock_models")

test_that("use", {

  testthat::expect_equal(
    length(
      beautier:::get_unlinked_clock_models(
        list(
          create_strict_clock_model(id = "a")
        )
      )
    ),
    1
  )

  testthat::expect_equal(
    length(
      beautier:::get_unlinked_clock_models(
        list(
          create_strict_clock_model(id = "a"),
          create_strict_clock_model(id = "b")
        )
      )
    ),
    2
  )

  testthat::expect_equal(
    length(
      beautier:::get_unlinked_clock_models(
        list(
          create_strict_clock_model(id = "a"),
          create_strict_clock_model(id = "a")
        )
      )
    ),
    1
  )

})
