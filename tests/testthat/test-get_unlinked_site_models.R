context("get_unlinked_site_models")

test_that("use", {

  testthat::expect_equal(
    length(
      beautier:::get_unlinked_site_models(
        list(
          create_jc69_site_model(id = "a")
        )
      )
    ),
    1
  )

  testthat::expect_equal(
    length(
      beautier:::get_unlinked_site_models(
        list(
          create_jc69_site_model(id = "a"),
          create_jc69_site_model(id = "b")
        )
      )
    ),
    2
  )

  testthat::expect_equal(
    length(
      beautier:::get_unlinked_site_models(
        list(
          create_jc69_site_model(id = "a"),
          create_jc69_site_model(id = "a")
        )
      )
    ),
    1
  )

})
