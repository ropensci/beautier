context("get_beautier_paths")

test_that("use", {

  testthat::expect_equal(
    c(
      beautier::get_beautier_path("anthus_nd2.fas"),
      beautier::get_beautier_path("anthus_nd3.fas")
    ),
    beautier::get_beautier_paths(c("anthus_nd2.fas", "anthus_nd3.fas"))
  )

})
