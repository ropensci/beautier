test_that("strict", {

  expected <- c(
    "<distribution id=\"treeLikelihood.test_output_0\" spec=\"ThreadedTreeLikelihood\" data=\"@test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">", # nolint XML
    "        <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "        <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "        <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "        <substModel id=\"JC69.s:test_output_0\" spec=\"JukesCantor\"/>", # nolint XML
    "    </siteModel>",
    "    <branchRateModel id=\"StrictClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint XML
    "        <parameter id=\"clockRate.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "    </branchRateModel>",
    "</distribution>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_tree_likelihood_distr_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})


test_that("RLN", {

  expected <- c(
    "<distribution id=\"treeLikelihood.test_output_0\" spec=\"ThreadedTreeLikelihood\" data=\"@test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">",
    "        <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "        <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "        <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "        <substModel id=\"JC69.s:test_output_0\" spec=\"JukesCantor\"/>", # nolint XML
    "    </siteModel>",
    "    <branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "        <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "            <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "        </LogNormal>",
    "        <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "    </branchRateModel>",
    "</distribution>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_tree_likelihood_distr_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("section of anthus_aco_sub_calibration.xml", {

  expected <- c(
    "<distribution id=\"treeLikelihood.anthus_aco_sub\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco_sub\" tree=\"@Tree.t:anthus_aco_sub\">", # nolint long line indeed
    "    <siteModel id=\"SiteModel.s:anthus_aco_sub\" spec=\"SiteModel\">",
    "        <parameter id=\"mutationRate.s:anthus_aco_sub\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "        <parameter id=\"gammaShape.s:anthus_aco_sub\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "        <parameter id=\"proportionInvariant.s:anthus_aco_sub\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint long line indeed
    "        <substModel id=\"JC69.s:anthus_aco_sub\" spec=\"JukesCantor\"/>",
    "    </siteModel>",
    "    <branchRateModel id=\"StrictClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.StrictClockModel\" clock.rate=\"@clockRate.c:anthus_aco_sub\"/>", # nolint long line indeed
    "</distribution>"
  )
  # This XML file has a normal distribution for the MRCA prior
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  inference_model <- init_inference_model(
    input_filename = fasta_filename,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      mcmc = create_mcmc(chain_length = 10000),
      mrca_prior = create_mrca_prior(
        name = "all",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        mrca_distr = create_normal_distr(
          id = 0,
          mean = create_mean_param(id = 1, value = "0.02"),
          sigma = create_sigma_param(id = 2, value = "0.001")
        ),
        is_monophyletic = TRUE,
        clock_prior_distr_id = 0
      ),
      beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
    )
  )

  created <- create_tree_likelihood_distr_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})


test_that("adapted from anthus_aco_sub_calibrated_no_prior.xml", {

  expected <- c(
    "<distribution id=\"treeLikelihood.anthus_aco_sub\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco_sub\" tree=\"@Tree.t:anthus_aco_sub\">", # nolint indeed a long line
    "    <siteModel id=\"SiteModel.s:anthus_aco_sub\" spec=\"SiteModel\">",
    "        <parameter id=\"mutationRate.s:anthus_aco_sub\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint indeed a long line
    "        <parameter id=\"gammaShape.s:anthus_aco_sub\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint indeed a long line
    "        <parameter id=\"proportionInvariant.s:anthus_aco_sub\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint indeed a long line
    "        <substModel id=\"JC69.s:anthus_aco_sub\" spec=\"JukesCantor\"/>",
    "    </siteModel>",
    "    <branchRateModel id=\"StrictClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint indeed a long line
    "        <parameter id=\"clockRate.c:anthus_aco_sub\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint indeed a long line
    "    </branchRateModel>",
    "</distribution>"
  )

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  inference_model <- init_inference_model(
    input_filename = fasta_filename,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      mcmc = create_mcmc(chain_length = 10000),
      mrca_prior = create_mrca_prior(
        name = "every",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        clock_prior_distr_id = 0
      ),
      beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
    )
  )
  created <- create_tree_likelihood_distr_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})





test_that("Adapted from anthus_aco_sub_20181016_all.xml", {

  expected <- c(
    "<distribution id=\"treeLikelihood.anthus_aco_sub\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco_sub\" tree=\"@Tree.t:anthus_aco_sub\">", # nolint long line indeed
    "    <siteModel id=\"SiteModel.s:anthus_aco_sub\" spec=\"SiteModel\">",
    "        <parameter id=\"mutationRate.s:anthus_aco_sub\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "        <parameter id=\"gammaShape.s:anthus_aco_sub\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "        <parameter id=\"proportionInvariant.s:anthus_aco_sub\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint long line indeed
    "        <substModel id=\"JC69.s:anthus_aco_sub\" spec=\"JukesCantor\"/>",
    "    </siteModel>",
    "    <branchRateModel id=\"StrictClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint long line indeed
    "        <parameter id=\"clockRate.c:anthus_aco_sub\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint long line indeed
    "    </branchRateModel>",
    "</distribution>"
  )
  fasta_filename <- "anthus_aco_sub.fas"
  inference_model <- init_inference_model(
    input_filename = get_beautier_path(fasta_filename),
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      mrca_prior = create_mrca_prior(
        name = "all",
        alignment_id = get_alignment_id(get_beautier_path(fasta_filename)),
        taxa_names = get_taxa_names(get_beautier_path(fasta_filename)),
        is_monophyletic = FALSE
      ),
      beauti_options = create_beauti_options(
        nucleotides_uppercase = TRUE,
        beast2_version = "2.5",
        required = "BEAST v2.5.0"
      )
    )
  )
  created <- create_tree_likelihood_distr_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("Adapted from anthus_aco_sub_20181016_all_monophyletic.xml", {

  expected <- c(
    "<distribution id=\"treeLikelihood.anthus_aco_sub\" spec=\"ThreadedTreeLikelihood\" data=\"@anthus_aco_sub\" tree=\"@Tree.t:anthus_aco_sub\">",
    "    <siteModel id=\"SiteModel.s:anthus_aco_sub\" spec=\"SiteModel\">",
    "        <parameter id=\"mutationRate.s:anthus_aco_sub\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>",
    "        <parameter id=\"gammaShape.s:anthus_aco_sub\" estimate=\"false\" name=\"shape\">1.0</parameter>",
    "        <parameter id=\"proportionInvariant.s:anthus_aco_sub\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>",
    "        <substModel id=\"JC69.s:anthus_aco_sub\" spec=\"JukesCantor\"/>",
    "    </siteModel>",
    "    <branchRateModel id=\"StrictClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">",
    "        <parameter id=\"clockRate.c:anthus_aco_sub\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>",
    "    </branchRateModel>",
    "</distribution>"
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      mrca_prior = create_mrca_prior(
        name = "all",
        alignment_id = get_alignment_id(get_beautier_path("anthus_aco_sub.fas")),
        taxa_names = get_taxa_names(get_beautier_path("anthus_aco_sub.fas")),
        is_monophyletic = TRUE
      ),
      beauti_options = create_beauti_options(
        nucleotides_uppercase = TRUE,
        beast2_version = "2.5",
        required = "BEAST v2.5.0"
      )
    )
  )
  created <- create_tree_likelihood_distr_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})
