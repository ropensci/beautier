context("get_alignment_ids")

test_that("use", {

  testthat::expect_equal(
    beautier:::get_alignment_ids(
      get_beautier_paths(c("anthus_aco.fas", "anthus_nd2.fas"))
    ),
    c(
      get_alignment_id(get_beautier_path("anthus_aco.fas")),
      get_alignment_id(get_beautier_path("anthus_nd2.fas"))
    )
  )

})
