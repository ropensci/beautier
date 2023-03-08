test_that("1: expose problem: files are different already", {
  # Verify these two files are equivalent
  #
  # * issue_135_no_mrca_no_estimate_beauti.xml
  # * issue_135_no_mrca_no_estimate_beautier.xml
  #
  # They are not, which is part of the problem.
  # The next test recreates the correct beautier file
  beauti_file <- beautier::get_beautier_path("issue_135_no_mrca_no_estimate_beauti.xml")
  beautier_file <- beautier::get_beautier_path("issue_135_no_mrca_no_estimate_beautier.xml")

  expect_false(beautier::are_equivalent_xml_files(beauti_file, beautier_file))
})

test_that("re-create file step by step 1/?: the minimal XML", {

  beauti_file <- beautier::get_beautier_path("anthus_aco_sub_2_6.xml")
  file.copy(from = beauti_file, to = "~/anthus_aco_sub_2_6.xml")

  fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")
  inference_model <- create_inference_model(
    beauti_options = beautier::create_beauti_options_v2_6(nucleotides_uppercase = TRUE)
  )

  # # Make the inference model match the BEAUti file
  inference_model$tree_prior$birth_rate_distr$id <- "1"
  # inference_model$site_model$kappa_prior_distr$m$id <- "1"
  # inference_model$site_model$kappa_prior_distr$s$id <- "2"

  beautier_file <- "~/created.xml"
  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model = inference_model
  )
  # If this passes, this is done!
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))
})

test_that("1: re-created file", {
  skip("Issue #135, Issue 135")
  # Re-create a beautier file that is equivalent to the BEAUti file
  # The BEAUti file is correct:
  # issue_135_no_mrca_no_estimate_beauti.xml

  # This is a unique file, delivered by the user
  beauti_file <- beautier::get_beautier_path("issue_135_no_mrca_no_estimate_beauti.xml")

  file.copy(beauti_file, "~/issue_135_no_mrca_no_estimate_beauti.xml")
  beautier_file <- "~/issue_135_no_mrca_no_estimate_beautier.xml"

  #without mrca prior, single value at clock rate
  fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")
  clock.rate <- beautier::create_clock_rate_param(value = 0.00277, estimate=FALSE)

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(id = NA, clock.rate),
    tree_prior = create_yule_tree_prior(),
    beauti_options = beautier::create_beauti_options_v2_6(nucleotides_uppercase = TRUE)
  )

  # Make the inference model match the BEAUti file
  inference_model$tree_prior$birth_rate_distr$id <- "1"
  inference_model$site_model$kappa_prior_distr$m$id <- "1"
  inference_model$site_model$kappa_prior_distr$s$id <- "2"

  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model = inference_model
  )
  # If this passes, this is done!
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))


  beauti_text <- readr::read_lines(beauti_file)
  beautier_text <- readr::read_lines(beautier_file)
  beautier::compare_lines(
    lines = beautier_text,
    expected = beauti_text,
    created_lines_filename = "~/created.xml",
    expected_lines_filename = "~/expected.xml"
  )

  # Third fix, only present in the issue_135_ files
  freq_param_regex <- "<operator id=.YuleBirthRateScaler.t:anthus_aco_sub. spec=.ScaleOperator. parameter=.@birthRate.t:anthus_aco_sub. weight=.3.0./>"
  expect_equal(1, sum(stringr::str_count(beauti_text, freq_param_regex)))
  expect_equal(1, sum(stringr::str_count(beautier_text, freq_param_regex)))

  # Fourth fix, only present in the issue_135_ files
  freq_prior_regex <- "<prior id=.FrequenciesPrior.s:anthus_aco_sub. name=.distribution. x=.@freqParameter.s:anthus_aco_sub.>"
  expect_equal(1, sum(stringr::str_count(beauti_text, freq_prior_regex)))
  expect_equal(1, sum(stringr::str_count(beautier_text, freq_prior_regex)))

  # First fix, works!
  kappa_param_regex <- "<parameter id=.kappa.s:anthus_aco_sub. spec=.parameter.RealParameter. lower=.0.0. name=.stateNode.>2.0</parameter>"
  expect_equal(1, sum(stringr::str_count(beauti_text, kappa_param_regex)))
  expect_equal(1, sum(stringr::str_count(beautier_text, kappa_param_regex)))

  # Second fix, works!
  freq_param_regex <- "<parameter id=.freqParameter.s:anthus_aco_sub. spec=.parameter.RealParameter. dimension=.4. lower=.0.0. name=.stateNode. upper=.1.0.>0.25</parameter>"
  expect_equal(1, sum(stringr::str_count(beauti_text, freq_param_regex)))
  expect_equal(1, sum(stringr::str_count(beautier_text, freq_param_regex)))
})

test_that("use", {
  skip("Issue #135, Issue 135")
  beauti_file <- beautier::get_beautier_path("issue_135_no_mrca_estimate_beauti.xml")
  beauti_text <- readr::read_lines(beauti_file)
  # <branchRateModel id="StrictClock.c:anthus_aco_sub" spec="beast.evolution.branchratemodel.StrictClockModel">                       # nolint
  #   <parameter id="clockRate.c:anthus_aco_sub" spec="parameter.RealParameter" estimate="true" name="clock.rate">0.0035</parameter>  # nolint
  # </branchRateModel>                                                                                                                # nolint
  beauti_line <- paste0(beauti_text, collapse = "")

  beautier_file <- beautier::get_beautier_path("issue_135_no_mrca_estimate_beautier.xml")
  beautier_text <- readr::read_lines(beautier_file)
  # <branchRateModel id="StrictClock.c:anthus_aco_sub" spec="beast.evolution.branchratemodel.StrictClockModel" clock.rate="@clockRate.c:anthus_aco_sub"/> # nolint
  beautier_line <- paste0(beautier_text, collapse = "")

  regex <- "<branchRateModel id=.StrictClock.c:anthus_aco_sub.[A-Za-z @\\\"=\\.:_]+"
  stringr::str_match(beauti_line, regex)
  expect_equal(2 * 2, 4)
})
