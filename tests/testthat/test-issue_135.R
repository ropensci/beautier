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

test_that("1: re-created file", {
  skip("Issue #135, Issue 135")
  # Re-create a beautier file that is equivalent to the BEAUti file
  # The BEAUti file is correct:
  # issue_135_no_mrca_no_estimate_beauti.xml
  beauti_file <- beautier::get_beautier_path("issue_135_no_mrca_no_estimate_beauti.xml")
  file.copy(beauti_file, "~/issue_135_no_mrca_no_estimate_beauti.xml")
  beautier_file <- "~/issue_135_no_mrca_no_estimate_beautier.xml"

  #without mrca prior, single value at clock rate
  fasta_filename <- babette::get_babette_path("anthus_aco_sub.fas")
  clock.rate <- beautier::create_clock_rate_param(value = 0.00277,estimate=FALSE)

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(id = NA,clock.rate),
    tree_prior = create_yule_tree_prior(),
    beauti_options = beautier::create_beauti_options_v2_6()
  )

  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model = inference_model
  )
  # If this passes, this is done!
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))

  # BEAUti text
  # <state id="state" spec="State" storeEvery="5000">
  #   <tree id="Tree.t:anthus_aco_sub" spec="beast.evolution.tree.Tree" name="stateNode">
  #   <taxonset id="TaxonSet.anthus_aco_sub" spec="TaxonSet">
  #   <alignment idref="anthus_aco_sub"/>
  #   </taxonset>
  #   </tree>
  #   <parameter id="kappa.s:anthus_aco_sub" spec="parameter.RealParameter" lower="0.0" name="stateNode">2.0</parameter>
  #   <parameter id="freqParameter.s:anthus_aco_sub" spec="parameter.RealParameter" dimension="4" lower="0.0" name="stateNode" upper="1.0">0.25</parameter>
  #   <parameter id="birthRate.t:anthus_aco_sub" spec="parameter.RealParameter" name="stateNode">1.0</parameter>
  # </state>

  beauti_text <- readr::read_lines(beauti_file)
  beautier_text <- readr::read_lines(beautier_file)
  kappa_param_regex <- "<parameter id=.kappa.s:anthus_aco_sub. spec=.parameter.RealParameter. lower=.0.0. name=.stateNode.>2.0</parameter>"
  expect_equal(1, sum(stringr::str_count(beauti_text, kappa_param_regex)))
  # First fix
  expect_equal(1, sum(stringr::str_count(beautier_text, kappa_param_regex)))

  freq_param_regex <- "<parameter id=.freqParameter.s:anthus_aco_sub. spec=.parameter.RealParameter. dimension=.4. lower=.0.0. name=.stateNode. upper=.1.0.>0.25</parameter>"
  expect_equal(1, sum(stringr::str_count(beauti_text, freq_param_regex)))
  # Second fix
  expect_equal(1, sum(stringr::str_count(beautier_text, freq_param_regex)))

  # Etc.

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
