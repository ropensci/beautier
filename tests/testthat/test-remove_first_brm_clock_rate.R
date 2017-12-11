context("remove_first_brm_clock_rate")

test_that("simplified single-line use anthus_aco", {

  input <- "<branchRateModel id=\"RelaxedClock.c:anthus_aco\" clock.rate=\"@ucldMean.c:anthus_aco\"/>" # nolint XML
  created <- beautier:::remove_first_brm_clock_rate(input)
  expected <- "<branchRateModel id=\"RelaxedClock.c:anthus_aco\"/>" # nolint XML
  testthat::expect_equal(created, expected)
})

test_that("simplified single-line use test_output_0", {

  input <- "<branchRateModel id=\"RelaxedClock.c:test_output_0\" clock.rate=\"@ucldMean.c:test_output_0\"/>" # nolint XML
  created <- beautier:::remove_first_brm_clock_rate(input)
  expected <- "<branchRateModel id=\"RelaxedClock.c:test_output_0\"/>" # nolint XML
  testthat::expect_equal(created, expected)
})

test_that("single-line use", {

  input <- "<branchRateModel id=\"RelaxedClock.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" clock.rate=\"@ucldMean.c:anthus_aco\" rateCategories=\"@rateCategories.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>" # nolint XML
  created <- beautier:::remove_first_brm_clock_rate(input)
  expected <- "<branchRateModel id=\"RelaxedClock.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>" # nolint XML
  testthat::expect_equal(created, expected)
})


test_that("multiline use", {

  # clock.rate put in anthus_aco
  input <- c(
    "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_aco\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco\" tree=\"@Tree.t:anthus_aco\">", # nolint XML
    "        <branchRateModel id=\"RelaxedClock.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" clock.rate=\"@ucldMean.c:anthus_aco\" rateCategories=\"@rateCategories.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>", # nolint XML
    "    </distribution>", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_nd2\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_nd2\" tree=\"@Tree.t:anthus_nd2\">", # nolint XML
    "        <branchRateModel id=\"RelaxedClock.c:anthus_nd2\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" clock.rate=\"@ucldMean.c:anthus_nd2\" rateCategories=\"@rateCategories.c:anthus_nd2\" tree=\"@Tree.t:anthus_nd2\"/>", # nolint XML
    "    </distribution>", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_nd3\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_nd3\" tree=\"@Tree.t:anthus_nd3\">", # nolint XML
    "        <branchRateModel id=\"RelaxedClock.c:anthus_nd3\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" clock.rate=\"@ucldMean.c:anthus_nd3\" rateCategories=\"@rateCategories.c:anthus_nd3\" tree=\"@Tree.t:anthus_nd3\"/>", # nolint XML
    "    </distribution>", # nolint XML
    "</distribution>" # nolint XML
  )
  created <- beautier:::remove_first_brm_clock_rate(input)
  expected <- c(
    "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_aco\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco\" tree=\"@Tree.t:anthus_aco\">", # nolint XML
    "        <branchRateModel id=\"RelaxedClock.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>", # nolint XML
    "    </distribution>", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_nd2\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_nd2\" tree=\"@Tree.t:anthus_nd2\">", # nolint XML
    "        <branchRateModel id=\"RelaxedClock.c:anthus_nd2\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" clock.rate=\"@ucldMean.c:anthus_nd2\" rateCategories=\"@rateCategories.c:anthus_nd2\" tree=\"@Tree.t:anthus_nd2\"/>", # nolint XML
    "    </distribution>", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_nd3\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_nd3\" tree=\"@Tree.t:anthus_nd3\">", # nolint XML
    "        <branchRateModel id=\"RelaxedClock.c:anthus_nd3\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" clock.rate=\"@ucldMean.c:anthus_nd3\" rateCategories=\"@rateCategories.c:anthus_nd3\" tree=\"@Tree.t:anthus_nd3\"/>", # nolint XML
    "    </distribution>", # nolint XML
    "</distribution>" # nolint XML
  )
  testthat::expect_equal(created, expected)
})
