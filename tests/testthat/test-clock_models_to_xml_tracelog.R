context("clock_models_to_xml_tracelog")

test_that("creates a text or NULL", {

  testthat::expect_true(
    is.null(
      beautier:::clock_models_to_xml_tracelog(
        create_strict_clock_models(ids = "anthus_aco")
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::clock_models_to_xml_tracelog(
        list(create_rln_clock_model(id = "anthus_aco"))
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::clock_models_to_xml_tracelog(
        create_strict_clock_models(ids = c("anthus_aco", "anthus_nd2"))
      )
    )
  )

})

################################################################################
# Single alignment
################################################################################

test_that("RLN", {

  expected <- c(
    "<log idref=\"ucldStdev.c:test_output_0\"/>", # nolint XML
    "<log id=\"rate.c:test_output_0\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:test_output_0\" tree=\"@Tree.t:test_output_0\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_rln_clock_model(id = "test_output_0")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict", {

  expected <- c(
    # Nothing
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_strict_clock_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Two alignments with unlinked clock models
################################################################################

test_that("RLN RLN ", {

  expected <- c(
    "<log idref=\"ucldStdev.c:anthus_aco\"/>", # nolint XML
    "<log id=\"rate.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"ucldMean.c:anthus_nd2\"/>", # nolint XML
    "<log idref=\"ucldStdev.c:anthus_nd2\"/>", # nolint XML
    "<log id=\"rate.c:anthus_nd2\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_nd2\" tree=\"@Tree.t:anthus_nd2\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_rln_clock_model(id = "anthus_aco"),
      create_rln_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("RLN strict", {

  expected <- c(
    "<log idref=\"clockRate.c:anthus_nd2\"/>", # nolint XML
    "<log idref=\"ucldStdev.c:anthus_aco\"/>", # nolint XML
    "<log id=\"rate.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_rln_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict RLN", {

  expected <- c(
    "<log idref=\"ucldMean.c:anthus_nd2\"/>", # nolint XML
    "<log idref=\"ucldStdev.c:anthus_nd2\"/>", # nolint XML
    "<log id=\"rate.c:anthus_nd2\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_nd2\" tree=\"@Tree.t:anthus_nd2\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_strict_clock_model(id = "anthus_aco"),
      create_rln_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict strict", {

  expected <- c(
    "<log idref=\"clockRate.c:anthus_nd2\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Two alignments with shared clock models
################################################################################

test_that("shared RLN", {

  expected <- c(
    "<log idref=\"ucldStdev.c:anthus_aco\"/>", # nolint XML
    "<log id=\"rate.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_rln_clock_model(id = "anthus_aco"),
      create_rln_clock_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("shared strict", {

  expected <- c(
    # Nothing
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
