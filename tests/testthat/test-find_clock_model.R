context("find_clock_model")

test_that("use", {

  testthat::expect_true(
    is_strict_clock_model(
      find_clock_model(
        clock_models = list(
          create_strict_clock_model(id = 1),
          create_rln_clock_model(id = 2)
        ),
        id = 1
      )
    )
  )

  testthat::expect_true(
    is_rln_clock_model(
      find_clock_model(
        clock_models = list(
          create_strict_clock_model(id = 1),
          create_rln_clock_model(id = 2)
        ),
        id = 2
      )
    )
  )

  testthat::expect_true(
    is.null(
      find_clock_model(
        clock_models = list(
          create_strict_clock_model(id = 1),
          create_rln_clock_model(id = 2)
        ),
        id = 3
      )
    )
  )

})

test_that("use", {

  testthat::expect_error(
    find_clock_model(
      clock_models = "nonsense",
      id = 1
    ),
    "'clock_models' must be a list of clock models"
  )

  testthat::expect_error(
    find_clock_model(
        clock_models = list(
          create_strict_clock_model(id = 1),
          create_rln_clock_model(id = 2)
        ),
      id = NA
    ),
    "'id' must be an ID"
  )

})
