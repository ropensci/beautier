context("clock_models_to_xml_tracelog")

test_that("strict", {

  expected <- c(
    # Nothing
  )
  created <- clock_models_to_xml_tracelog(
    list(
      create_strict_clock_model(id = "anthus_aco")
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln", {

  expected <- c(
    "<log idref=\"ucldStdev.c:test_output_0\"/>", # nolint XML
    "<log id=\"rate.c:test_output_0\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:test_output_0\" tree=\"@Tree.t:test_output_0\"/>" # nolint XML
  )
  created <- clock_models_to_xml_tracelog(
    list(
      create_rln_clock_model(id = "test_output_0")
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln + mrca with distr", {

  # From 'rln_mrca_one_div_x_2_5.xml'
  expected <- c(
    "<log idref=\"ucldStdev.c:anthus_aco_sub\"/>",
    "<log id=\"rate.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_aco_sub\" tree=\"@Tree.t:anthus_aco_sub\"/>",
    # "<log idref=\"all_taxa.prior\"/>",
    "<log idref=\"ucldMean.c:anthus_aco_sub\"/>"
  )
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  created <- clock_models_to_xml_tracelog(
    list(
      create_rln_clock_model(id = "anthus_aco_sub")
    ),
    mrca_priors = list(
      create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        mrca_distr = create_one_div_x_distr()
      )
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
