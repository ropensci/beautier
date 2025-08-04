test_that("use", {
  # True
  beast2_input_file <- get_beautier_path("babette_issue_109_expected_v2_6.xml")
  expect_true(is_beast2_input_file_with_tipdates(beast2_input_file))

  beast2_input_file <- get_beautier_path("G_VII_pre2003.xml")
  expect_true(is_beast2_input_file_with_tipdates(beast2_input_file))

  beast2_input_file <- get_beautier_path("tipdates_2_6.xml")
  expect_true(is_beast2_input_file_with_tipdates(beast2_input_file))

  beast2_input_file <- get_beautier_path("Felinecoronavirus_Envelope_1.xml")
  expect_true(is_beast2_input_file_with_tipdates(beast2_input_file))


  # False
  beast2_input_file <- get_beautier_path("2_4.xml")
  expect_false(is_beast2_input_file_with_tipdates(beast2_input_file))

  beast2_input_file <- get_beautier_path("anthus_aco_sub_20181016.xml")
  expect_false(is_beast2_input_file_with_tipdates(beast2_input_file))

  beast2_input_file <- get_beautier_path("anthus_aco_sub_2_5_1.xml")
  expect_false(is_beast2_input_file_with_tipdates(beast2_input_file))

  beast2_input_file <- get_beautier_path("anthus_aco_sub_2_6.xml")
  expect_false(is_beast2_input_file_with_tipdates(beast2_input_file))

  beast2_input_file <- get_beautier_path("gtr_2_4.xml")
  expect_false(is_beast2_input_file_with_tipdates(beast2_input_file))

  beast2_input_file <- get_beautier_path("gtr_gcc_1_2_4.xml")
  expect_false(is_beast2_input_file_with_tipdates(beast2_input_file))
})
