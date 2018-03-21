context("create_beast2_input_distr_lh")

################################################################################
# One alignment
################################################################################

test_that("strict", {

  expected <- c(
    "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
    "    <distribution id=\"treeLikelihood.test_output_0\" spec=\"ThreadedTreeLikelihood\" data=\"@test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">", # nolint XML
    "            <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:test_output_0\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>",
    "        <branchRateModel id=\"StrictClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint XML
    "            <parameter id=\"clockRate.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "        </branchRateModel>",
    "    </distribution>",
    "</distribution>"
  )

  created <- beautier:::create_beast2_input_distr_lh(
    site_models = list(
      create_jc69_site_model(id = "test_output_0")
    ),
    clock_models = list(
      create_strict_clock_model(id = "test_output_0")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
  testthat::expect_true(beautier:::is_xml(created))
  testthat::expect_true(beautier:::is_xml(expected))
})


test_that("RLN", {

  expected <- c(
    "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
    "    <distribution id=\"treeLikelihood.test_output_0\" spec=\"ThreadedTreeLikelihood\" data=\"@test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">",
    "            <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:test_output_0\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>",
    "        <branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "            <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "                <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "            </LogNormal>",
    "            <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "        </branchRateModel>",
    "    </distribution>",
    "</distribution>"
  )
  created <- beautier:::create_beast2_input_distr_lh(
    site_models = list(
      create_jc69_site_model(id = "test_output_0")
    ),
    clock_models = list(
      create_rln_clock_model(
        id = "test_output_0",
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

################################################################################
# Two alignments, clock models
################################################################################

test_that("strict strict", {

  expected <- c(
    "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_aco\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco\" tree=\"@Tree.t:anthus_aco\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:anthus_aco\" spec=\"SiteModel\">", # nolint XML
    "            <parameter id=\"mutationRate.s:anthus_aco\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:anthus_aco\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:anthus_aco\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:anthus_aco\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>", # nolint XML
    "        <branchRateModel id=\"StrictClock.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint XML
    "            <parameter id=\"clockRate.c:anthus_aco\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "        </branchRateModel>", # nolint XML
    "    </distribution>", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_nd2\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_nd2\" tree=\"@Tree.t:anthus_nd2\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:anthus_nd2\" spec=\"SiteModel\">", # nolint XML
    "            <parameter id=\"mutationRate.s:anthus_nd2\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:anthus_nd2\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:anthus_nd2\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:anthus_nd2\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>", # nolint XML
    "        <branchRateModel id=\"StrictClock.c:anthus_nd2\" spec=\"beast.evolution.branchratemodel.StrictClockModel\" clock.rate=\"@clockRate.c:anthus_nd2\"/>", # nolint XML
    "    </distribution>", # nolint XML
    "</distribution>" # nolint XML
  )
  created <- beautier:::create_beast2_input_distr_lh(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco"),
      create_jc69_site_model(id = "anthus_nd2")
    ),
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict RLN", {

  input_filenames <- beautier::get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  ids <- beautier:::get_ids(input_filenames)

  expected <- c(
        "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
        "    <distribution id=\"treeLikelihood.anthus_aco\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco\" tree=\"@Tree.t:anthus_aco\">", # nolint XML
        "        <siteModel id=\"SiteModel.s:anthus_aco\" spec=\"SiteModel\">", # nolint XML
        "            <parameter id=\"mutationRate.s:anthus_aco\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
        "            <parameter id=\"gammaShape.s:anthus_aco\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
        "            <parameter id=\"proportionInvariant.s:anthus_aco\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
        "            <substModel id=\"JC69.s:anthus_aco\" spec=\"JukesCantor\"/>", # nolint XML
        "        </siteModel>", # nolint XML
        "        <branchRateModel id=\"StrictClock.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint XML
        "            <parameter id=\"clockRate.c:anthus_aco\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
        "        </branchRateModel>", # nolint XML
        "    </distribution>", # nolint XML
        "    <distribution id=\"treeLikelihood.anthus_nd2\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_nd2\" tree=\"@Tree.t:anthus_nd2\">", # nolint XML
        "        <siteModel id=\"SiteModel.s:anthus_nd2\" spec=\"SiteModel\">", # nolint XML
        "            <parameter id=\"mutationRate.s:anthus_nd2\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
        "            <parameter id=\"gammaShape.s:anthus_nd2\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
        "            <parameter id=\"proportionInvariant.s:anthus_nd2\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
        "            <substModel id=\"JC69.s:anthus_nd2\" spec=\"JukesCantor\"/>", # nolint XML
        "        </siteModel>", # nolint XML
        "        <branchRateModel id=\"RelaxedClock.c:anthus_nd2\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" clock.rate=\"@ucldMean.c:anthus_nd2\" rateCategories=\"@rateCategories.c:anthus_nd2\" tree=\"@Tree.t:anthus_nd2\">", # nolint XML
        "            <LogNormal id=\"LogNormalDistributionModel.c:anthus_nd2\" S=\"@ucldStdev.c:anthus_nd2\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
        "                <parameter id=\"RealParameter.2\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
        "            </LogNormal>",
        "        </branchRateModel>",
        "    </distribution>",
        "</distribution>"
  )
  created <- beautier:::create_beast2_input_distr_lh(
    site_models = create_jc69_site_models(ids = ids),
    clock_models = list(
      create_strict_clock_model(
        id = ids[1]
      ),
      create_rln_clock_model(
        id = ids[2],
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 2
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("RLN strict", {

  expected <- c(
    "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_aco\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco\" tree=\"@Tree.t:anthus_aco\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:anthus_aco\" spec=\"SiteModel\">", # nolint XML
    "            <parameter id=\"mutationRate.s:anthus_aco\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:anthus_aco\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:anthus_aco\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:anthus_aco\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>", # nolint XML
    "        <branchRateModel id=\"RelaxedClock.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\">", # nolint XML
    "            <LogNormal id=\"LogNormalDistributionModel.c:anthus_aco\" S=\"@ucldStdev.c:anthus_aco\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "                <parameter id=\"RealParameter.20\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "            </LogNormal>", # nolint XML
    "            <parameter id=\"ucldMean.c:anthus_aco\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "        </branchRateModel>", # nolint XML
    "    </distribution>", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_nd2\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_nd2\" tree=\"@Tree.t:anthus_nd2\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:anthus_nd2\" spec=\"SiteModel\">", # nolint XML
    "            <parameter id=\"mutationRate.s:anthus_nd2\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:anthus_nd2\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:anthus_nd2\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:anthus_nd2\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>", # nolint XML
    "        <branchRateModel id=\"StrictClock.c:anthus_nd2\" spec=\"beast.evolution.branchratemodel.StrictClockModel\" clock.rate=\"@clockRate.c:anthus_nd2\"/>", # nolint XML
    "    </distribution>", # nolint XML
    "</distribution>"
  )
  created <- beautier:::create_beast2_input_distr_lh(
    site_models = create_jc69_site_models(ids = c("anthus_aco", "anthus_nd2")),
    clock_models = list(
      create_rln_clock_model(
        id = "anthus_aco",
        mparam_id = 20
      ),
      create_strict_clock_model(
        id = "anthus_nd2"
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})


test_that("RLN RLN", {

  expected <- c(
    "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_aco\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco\" tree=\"@Tree.t:anthus_aco\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:anthus_aco\" spec=\"SiteModel\">", # nolint XML
    "            <parameter id=\"mutationRate.s:anthus_aco\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:anthus_aco\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:anthus_aco\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:anthus_aco\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>", # nolint XML
    "        <branchRateModel id=\"RelaxedClock.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\">", # nolint XML
    "            <LogNormal id=\"LogNormalDistributionModel.c:anthus_aco\" S=\"@ucldStdev.c:anthus_aco\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "                <parameter id=\"RealParameter.20\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "            </LogNormal>", # nolint XML
    "            <parameter id=\"ucldMean.c:anthus_aco\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "        </branchRateModel>", # nolint XML
    "    </distribution>", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_nd2\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_nd2\" tree=\"@Tree.t:anthus_nd2\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:anthus_nd2\" spec=\"SiteModel\">", # nolint XML
    "            <parameter id=\"mutationRate.s:anthus_nd2\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:anthus_nd2\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:anthus_nd2\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:anthus_nd2\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>", # nolint XML
    "        <branchRateModel id=\"RelaxedClock.c:anthus_nd2\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" clock.rate=\"@ucldMean.c:anthus_nd2\" rateCategories=\"@rateCategories.c:anthus_nd2\" tree=\"@Tree.t:anthus_nd2\">", # nolint XML
    "            <LogNormal id=\"LogNormalDistributionModel.c:anthus_nd2\" S=\"@ucldStdev.c:anthus_nd2\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "                <parameter id=\"RealParameter.63\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "            </LogNormal>", # nolint XML
    "        </branchRateModel>", # nolint XML
    "    </distribution>", # nolint XML
    "</distribution>" # nolint XML
  )
  created <- beautier:::create_beast2_input_distr_lh(
    site_models = create_jc69_site_models(
      ids = c("anthus_aco", "anthus_nd2")
    ),
    clock_models = list(
      create_rln_clock_model(
        id = "anthus_aco",
        mparam_id = 20
      ),
      create_rln_clock_model(
        id = "anthus_nd2",
        mparam_id = 63
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict shared", {

  expected <- c(
    "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_aco\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco\" tree=\"@Tree.t:anthus_aco\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:anthus_aco\" spec=\"SiteModel\">", # nolint XML
    "            <parameter id=\"mutationRate.s:anthus_aco\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:anthus_aco\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:anthus_aco\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:anthus_aco\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>", # nolint XML
    "        <branchRateModel id=\"StrictClock.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint XML
    "            <parameter id=\"clockRate.c:anthus_aco\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "        </branchRateModel>", # nolint XML
    "    </distribution>", # nolint XML
    "    <distribution id=\"treeLikelihood.anthus_nd2\" spec=\"ThreadedTreeLikelihood\" branchRateModel=\"@StrictClock.c:anthus_aco\" data=\"@anthus_nd2\" tree=\"@Tree.t:anthus_nd2\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:anthus_nd2\" spec=\"SiteModel\">", # nolint XML
    "            <parameter id=\"mutationRate.s:anthus_nd2\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:anthus_nd2\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:anthus_nd2\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:anthus_nd2\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>", # nolint XML
    "    </distribution>", # nolint XML
    "</distribution>" # nolint XML
  )
  created <- beautier:::create_beast2_input_distr_lh(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco"),
      create_jc69_site_model(id = "anthus_nd2")
    ),
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
  testthat::expect_true(beautier:::is_xml(created))
  testthat::expect_true(beautier:::is_xml(expected))
})

test_that("RLN shared", {

  # Cannot have a shared RLN clock
  testthat::expect_error(
    beautier:::create_beast2_input_distr_lh(
      site_models = list(
        create_jc69_site_model(id = "anthus_aco"),
        create_jc69_site_model(id = "anthus_nd2")
      ),
      clock_models = list(
        create_rln_clock_model(id = "anthus_aco"),
        create_rln_clock_model(id = "anthus_aco")
      )
    )
  )
})

test_that("Must not have two branchRateModels, #26", {

  fasta_filename <- get_fasta_filename()
  created <- beautier:::create_beast2_input_distr_lh(
    site_models = list(
      create_jc69_site_model(id = get_alignment_id(fasta_filename))
    ),
    clock_models = list(
      create_rln_clock_model(id = get_alignment_id(fasta_filename))
    ),
    mrca_priors = list(
      create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        is_monophyletic = TRUE
      )
    )
  )
  testthat::expect_equal(
    1,
    sum(grepl(x = created, pattern = " *<branchRateModel.*"))
  )

})



################################################################################
# Two alignments, site models
################################################################################
