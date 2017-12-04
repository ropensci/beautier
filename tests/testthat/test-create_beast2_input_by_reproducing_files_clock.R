context(
  paste(
    "create_beast2_input by reproducing files,",
    "multiple alignments with combinations of clock models"
  )
)

################################################################################
# Clock models
################################################################################

################################################################################
# Clock model: RLN
################################################################################

test_that("rln_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    clock_models = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "rln_2_4.xml", package = "beautier"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("rln_uclstdev_beta_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    clock_models = create_rln_clock_model(
      ucldstdev_distr = create_beta_distr(
        id = 0,
        alpha = create_alpha_param(id = 4, value = "2.0"),
        beta = create_beta_param(id = 5, value = "2.0")
      ),
      mparam_id = 1
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )
  expected_lines <- readLines(system.file("extdata",
    "rln_uclstdev_beta_2_4.xml", package = "beautier"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Clock model: strict
################################################################################

test_that("strict_clock_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "strict_clock_2_4.xml", package = "beautier"))

  skip("WIP: state section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("strict_clock_rate_0_5_2_4.xml", {

  input_fasta_filename <- beautier::get_input_fasta_filename()
  id <- get_id(input_fasta_filename)
  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    clock_models = create_strict_clock_model(
      clock_rate_param = create_clock_rate_param(
        id = id,
        value = "0.5"
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "strict_clock_rate_0_5_2_4.xml", package = "beautier"))

  skip("WIP: state section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Two alignments
################################################################################

test_that("aco_nd2_strict_rln_2_4.xml, example 10", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  input_fasta_filenames <- c(fasta_filename_1, fasta_filename_2)

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    clock_models = list(
      create_strict_clock_model(),
      create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 3, value = "0.5396"),
          beta = create_beta_param(id = 4, value = "0.3819")
        )
      )
    ),
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 4))
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_strict_rln_2_4.xml", package = "beautier"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})

test_that("aco_nd2_rln_rln_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    clock_models = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_rln_rln_2_4.xml", package = "beautier"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})


################################################################################
# Three alignments
################################################################################


################################################################################
# Four alignments
################################################################################

test_that("aco_nd2_nd3_nd4_shared_clock_2_4.xml", {

  skip("WIP: interface")

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_3 <- system.file("extdata",
    "anthus_nd3.fas", package = "beautier")
  fasta_filename_4 <- system.file("extdata",
    "anthus_nd4.fas", package = "beautier")
  input_fasta_filenames <- c(
    fasta_filename_1, fasta_filename_2, fasta_filename_3, fasta_filename_4
  )

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    clock_models = create_strict_clock_model(get_id(input_fasta_filenames[1])),
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 111)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 222)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 333)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 444))
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_nd3_nd4_shared_clock_2_4.xml", package = "beautier"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})