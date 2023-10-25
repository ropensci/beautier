test_that("use", {

  expect_equal(
    get_n_taxa(get_beautier_path("test_output_5.fas")),
    5
  )

  expect_equal(
    get_n_taxa(get_beautier_path("test_output_6.fas")),
    6
  )

  expect_equal(
    get_n_taxa(get_beautier_path("anthus_aco.fas")),
    22
  )

  expect_silent(
    get_n_taxa(get_beautier_path("anthus_nd2.fas"))
  )

  expect_equal(
    get_n_taxa(get_beautier_path("anthus_nd2.fas")),
    22
  )

  expect_equal(
    get_n_taxa(get_beautier_path("anthus_nd3.fas")),
    22
  )

  expect_equal(
    get_n_taxa(get_beautier_path("anthus_nd4.fas")),
    22
  )

})

test_that("abuse", {

  expect_error(
    get_n_taxa(filename = c("one_too", "many")),
    "filename"
  )

  expect_error(
    get_n_taxa("abs.ent"),
    "filename must exist"
  )

  expect_error(
    get_n_taxa(get_beautier_path("2_4.xml")),
    "filename"
  )

})
