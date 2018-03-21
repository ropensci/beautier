context("get_n_taxa")

test_that("use", {

  testthat::expect_equal(
    beautier:::get_n_taxa(beautier::get_beautier_path("test_output_5.fas")),
    5
  )

  testthat::expect_equal(
    beautier:::get_n_taxa(beautier::get_beautier_path("test_output_6.fas")),
    6
  )

  testthat::expect_equal(
    beautier:::get_n_taxa(beautier::get_beautier_path("anthus_aco.fas")),
    22
  )

  testthat::expect_silent(
    beautier:::get_n_taxa(beautier::get_beautier_path("anthus_nd2.fas"))
  )

  testthat::expect_equal(
    beautier:::get_n_taxa(beautier::get_beautier_path("anthus_nd2.fas")),
    22
  )

  testthat::expect_equal(
    beautier:::get_n_taxa(beautier::get_beautier_path("anthus_nd3.fas")),
    22
  )

  testthat::expect_equal(
    beautier:::get_n_taxa(beautier::get_beautier_path("anthus_nd4.fas")),
    22
  )

})

test_that("abuse", {

  testthat::expect_error(
    beautier:::get_n_taxa("abs.end"),
    "'filename' must be the name of a file that is present"
  )

})
