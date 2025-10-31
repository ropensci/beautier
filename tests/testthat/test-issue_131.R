################################################################################
# Defaults for different versions
test_that("issue_131", {
  skip("Not done yet")
  created <- create_beast2_input(
    input_filename = get_fasta_filename(),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    beauti_options = create_beauti_options_v2_4()
  )

  # Use BEAti from this version:
  beastierinstall::install_beast2()

  # The file must be in 'inst/extdata'
  expected <- readLines(get_beautier_path("issue_131.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})
