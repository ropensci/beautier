test_that("creates a text", {
  expect_true(
    is.null(
      clock_model_to_xml_tracelog(
        create_inference_model(
          clock_model = create_strict_clock_model(id = "anthus_aco")
        )
      )
    )
  )
})

test_that("rln", {
  expected <- c(
    "<log idref=\"ucldStdev.c:test_output_0\"/>", # nolint XML
    "<log id=\"rate.c:test_output_0\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:test_output_0\" tree=\"@Tree.t:test_output_0\"/>" # nolint XML
  )
  created <- clock_model_to_xml_tracelog(
    inference_model = create_inference_model(
      clock_model = create_rln_clock_model(id = "test_output_0")
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln + mrca with distr", {
  # From 'rln_mrca_one_div_x_2_5.xml'
  expected <- c(
    "<log idref=\"ucldStdev.c:anthus_aco_sub\"/>", # nolint this is no absolute path
    "<log id=\"rate.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_aco_sub\" tree=\"@Tree.t:anthus_aco_sub\"/>", # nolint XML
    # "<log idref=\"all_taxa.prior\"/>",
    "<log idref=\"ucldMean.c:anthus_aco_sub\"/>" # nolint this is no absolute path
  )
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  created <- clock_model_to_xml_tracelog(
    create_inference_model(
      clock_model = create_rln_clock_model(id = "anthus_aco_sub"),
      mrca_prior = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        mrca_distr = create_one_div_x_distr()
      )
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
