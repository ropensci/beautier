test_that("use", {

  expect_equal(
    system.file("extdata", "anthus_nd2.fas", package = "beautier"),
    beautier::get_beautier_path("anthus_nd2.fas")
  )

  expect_equal(
    system.file("extdata", "anthus_aco.fas", package = "beautier"),
    beautier::get_beautier_path("anthus_aco.fas")
  )

})

test_that("abuse", {

  expect_error(
    beautier::get_beautier_path("abs.ent"),
    "'filename' must be the name of a file in 'inst/extdata'"
  )

})
