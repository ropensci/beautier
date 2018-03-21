context("clock_models_to_xml_prior_distr")

################################################################################
# One alignment
################################################################################

test_that("RLN", {

  expected <- c(
    "<prior id=\"ucldStdevPrior.c:test_output_0\" name=\"distribution\" x=\"@ucldStdev.c:test_output_0\">", # nolint XML
    "    <Gamma id=\"Gamma.0\" name=\"distr\">",
    "        <parameter id=\"RealParameter.2\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.3\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>"

  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
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

test_that("strict", {

  testthat::expect_true(
    is.null(
      beautier:::clock_models_to_xml_prior_distr(
        list(create_strict_clock_model()) # Don't even need an ID
      )
    )
  )
})


################################################################################
# Two alignments, unlinked clock models
################################################################################

test_that("RLN RLN", {

  expected <- c(
    "<prior id=\"ucldStdevPrior.c:anthus_aco\" name=\"distribution\" x=\"@ucldStdev.c:anthus_aco\">", # nolint XML
    "    <Gamma id=\"Gamma.6\" name=\"distr\">",
    "        <parameter id=\"RealParameter.21\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.22\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_nd2\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd2\">", # nolint XML
    "    <Gamma id=\"Gamma.14\" name=\"distr\">",
    "        <parameter id=\"RealParameter.64\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.65\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd2\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd2\">", # nolint XML
    "    <Uniform id=\"Uniform.14\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>"
  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_rln_clock_model(
        id = "anthus_aco",
        ucldstdev_distr = create_gamma_distr(
          id = 6,
          alpha = create_alpha_param(id = 21, value = "0.5396"),
          beta = create_beta_param(id = 22, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = "irrelevant")
      ),
      create_rln_clock_model(
        id = "anthus_nd2",
        ucldstdev_distr = create_gamma_distr(
          id = 14,
          alpha = create_alpha_param(id = 64, value = "0.5396"),
          beta = create_beta_param(id = 65, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 14)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("RLN strict", {

  expected <- c(
    "<prior id=\"ClockPrior.c:anthus_nd2\" name=\"distribution\" x=\"@clockRate.c:anthus_nd2\">", # nolint XML
    "    <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_aco\" name=\"distribution\" x=\"@ucldStdev.c:anthus_aco\">", # nolint XML
    "    <Gamma id=\"Gamma.6\" name=\"distr\">",
    "        <parameter id=\"RealParameter.21\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.22\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>"

  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_rln_clock_model(
        id = "anthus_aco",
        ucldstdev_distr = create_gamma_distr(
          id = 6,
          alpha = create_alpha_param(id = 21, value = "0.5396"),
          beta = create_beta_param(id = 22, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = "irrelevant")
      ),
      create_strict_clock_model(
        id = "anthus_nd2",
        clock_rate_distr = create_uniform_distr(id = 3)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict RLN", {

  expected <- c(
    "<prior id=\"ucldStdevPrior.c:anthus_nd2\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd2\">", # nolint XML
    "    <Gamma id=\"Gamma.0\" name=\"distr\">",
    "        <parameter id=\"RealParameter.3\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.4\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd2\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd2\">", # nolint XML
    "    <Uniform id=\"Uniform.6\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>"
  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_strict_clock_model(
        id = "anthus_aco",
        clock_rate_distr = create_uniform_distr(id = 6)
      ),
      create_rln_clock_model(
        id = "anthus_nd2",
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 3, value = "0.5396"),
          beta = create_beta_param(id = 4, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 6)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})


test_that("strict strict", {

  expected <- c(
    "<prior id=\"ClockPrior.c:anthus_nd2\" name=\"distribution\" x=\"@clockRate.c:anthus_nd2\">", # nolint XML
    "    <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>"
  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_strict_clock_model(
        id = "anthus_aco",
        clock_rate_distr = create_uniform_distr(id = "irrelevant")
      ),
      create_strict_clock_model(
        id = "anthus_nd2",
        clock_rate_distr = create_uniform_distr(id = 3)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Two alignments, linked clock models
################################################################################

test_that("RLN shared", {

  expected <- c(
    "<prior id=\"ucldStdevPrior.c:anthus_aco\" name=\"distribution\" x=\"@ucldStdev.c:anthus_aco\">", # nolint XML
    "    <Gamma id=\"Gamma.6\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.21\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.22\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>", # nolint XML
    "</prior>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_rln_clock_model(
        id = "anthus_aco",
        ucldstdev_distr = create_gamma_distr(
          id = 6,
          alpha = create_alpha_param(id = 21, value = "0.5396"),
          beta = create_beta_param(id = 22, value = "0.3819")
        )
      ),
      create_rln_clock_model(
        id = "anthus_aco"
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict shared", {

  testthat::expect_true(
    is.null(
      beautier:::clock_models_to_xml_prior_distr(
        list(
          create_strict_clock_model(id = "anthus_aco"),
          create_strict_clock_model(id = "anthus_aco")
        )
      )
    )
  )
})

################################################################################
# Four alignments
################################################################################

test_that("RLN RLN RLN RLN", {

  expected <- c(
    "<prior id=\"ucldStdevPrior.c:anthus_aco\" name=\"distribution\" x=\"@ucldStdev.c:anthus_aco\">", # nolint XML
    "    <Gamma id=\"Gamma.6\" name=\"distr\">",
    "        <parameter id=\"RealParameter.21\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.22\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    # Note there is no 'MeanRatePrior.c' for the first element
    "<prior id=\"ucldStdevPrior.c:anthus_nd2\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd2\">", # nolint XML
    "    <Gamma id=\"Gamma.14\" name=\"distr\">",
    "        <parameter id=\"RealParameter.64\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.65\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd2\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd2\">", # nolint XML
    "    <Uniform id=\"Uniform.14\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_nd3\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd3\">", # nolint XML
    "    <Gamma id=\"Gamma.16\" name=\"distr\">",
    "        <parameter id=\"RealParameter.87\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.88\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd3\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd3\">", # nolint XML
    "    <Uniform id=\"Uniform.23\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_nd4\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd4\">", # nolint XML
    "    <Gamma id=\"Gamma.17\" name=\"distr\">",
    "        <parameter id=\"RealParameter.91\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.92\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd4\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd4\">", # nolint XML
    "    <Uniform id=\"Uniform.27\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>"

  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_rln_clock_model(
        id = "anthus_aco",
        ucldstdev_distr = create_gamma_distr(
          id = 6,
          alpha = create_alpha_param(id = 21, value = "0.5396"),
          beta = create_beta_param(id = 22, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1
      ),
      create_rln_clock_model(
        id = "anthus_nd2",
        ucldstdev_distr = create_gamma_distr(
          id = 14,
          alpha = create_alpha_param(id = 64, value = "0.5396"),
          beta = create_beta_param(id = 65, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 14),
        mparam_id = 1
      ),
      create_rln_clock_model(
        id = "anthus_nd3",
        ucldstdev_distr = create_gamma_distr(
          id = 16,
          alpha = create_alpha_param(id = 87, value = "0.5396"),
          beta = create_beta_param(id = 88, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 14),
        mparam_id = 1
      ),
      create_rln_clock_model(
        id = "anthus_nd4",
        ucldstdev_distr = create_gamma_distr(
          id = 17,
          alpha = create_alpha_param(id = 91, value = "0.5396"),
          beta = create_beta_param(id = 92, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 27),
        mparam_id = 1
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict strict strict strict", {

  expected <- c(
    "<prior id=\"ClockPrior.c:anthus_nd4\" name=\"distribution\" x=\"@clockRate.c:anthus_nd4\">", # nolint XML
    "    <Uniform id=\"Uniform.24\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ClockPrior.c:anthus_nd3\" name=\"distribution\" x=\"@clockRate.c:anthus_nd3\">", # nolint XML
    "    <Uniform id=\"Uniform.20\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ClockPrior.c:anthus_nd2\" name=\"distribution\" x=\"@clockRate.c:anthus_nd2\">", # nolint XML
    "    <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>"
  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_strict_clock_model(
        id = "anthus_aco",
        clock_rate_distr = create_uniform_distr(id = "irrelevant")
      ),
      create_strict_clock_model(
        id = "anthus_nd2",
        clock_rate_distr = create_uniform_distr(id = 3)
      ),
      create_strict_clock_model(
        id = "anthus_nd3",
        clock_rate_distr = create_uniform_distr(id = 20)
      ),
      create_strict_clock_model(
        id = "anthus_nd4",
        clock_rate_distr = create_uniform_distr(id = 24)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("RLN RLN strict strict", {

  expected <- c(
    "<prior id=\"ClockPrior.c:anthus_nd4\" name=\"distribution\" x=\"@clockRate.c:anthus_nd4\">", # nolint XML
    "    <Uniform id=\"Uniform.24\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ClockPrior.c:anthus_nd3\" name=\"distribution\" x=\"@clockRate.c:anthus_nd3\">", # nolint XML
    "    <Uniform id=\"Uniform.20\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_aco\" name=\"distribution\" x=\"@ucldStdev.c:anthus_aco\">", # nolint XML
    "    <Gamma id=\"Gamma.6\" name=\"distr\">",
    "        <parameter id=\"RealParameter.21\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.22\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_nd2\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd2\">", # nolint XML
    "    <Gamma id=\"Gamma.14\" name=\"distr\">",
    "        <parameter id=\"RealParameter.64\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.65\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd2\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd2\">", # nolint XML
    "    <Uniform id=\"Uniform.14\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>"
  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_rln_clock_model(
        id = "anthus_aco",
        mean_rate_prior_distr = create_uniform_distr(id = "irrelevant"),
        ucldstdev_distr = create_gamma_distr(
          id = 6,
          alpha = create_alpha_param(id = 21, value = "0.5396"),
          beta = create_beta_param(id = 22, value = "0.3819")
        )
      ),
      create_rln_clock_model(
        id = "anthus_nd2",
        mean_rate_prior_distr = create_uniform_distr(id = 14),
        ucldstdev_distr = create_gamma_distr(
          id = 14,
          alpha = create_alpha_param(id = 21, value = "0.5396"),
          beta = create_beta_param(id = 22, value = "0.3819")
        )
      ),
      create_strict_clock_model(
        id = "anthus_nd3",
        clock_rate_distr = create_uniform_distr(id = 20)
      ),
      create_strict_clock_model(
        id = "anthus_nd4",
        clock_rate_distr = create_uniform_distr(id = 24)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})






test_that("strict RLN RLN strict", {

  expected <- c(
    "<prior id=\"ClockPrior.c:anthus_nd4\" name=\"distribution\" x=\"@clockRate.c:anthus_nd4\">", # nolint XML
    "    <Uniform id=\"Uniform.24\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_nd2\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd2\">", # nolint XML
    "    <Gamma id=\"Gamma.14\" name=\"distr\">",
    "        <parameter id=\"RealParameter.64\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.65\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd2\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd2\">", # nolint XML
    "    <Uniform id=\"Uniform.14\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_nd3\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd3\">", # nolint XML
    "    <Gamma id=\"Gamma.22\" name=\"distr\">",
    "        <parameter id=\"RealParameter.106\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.107\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd3\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd3\">", # nolint XML
    "    <Uniform id=\"Uniform.26\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>"
  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_strict_clock_model(
        id = "anthus_aco",
        clock_rate_distr = create_uniform_distr(id = "irrelevant")
      ),
      create_rln_clock_model(
        id = "anthus_nd2",
        mean_rate_prior_distr = create_uniform_distr(id = 14),
        ucldstdev_distr = create_gamma_distr(
          id = 14,
          alpha = create_alpha_param(id = 64, value = "0.5396"),
          beta = create_beta_param(id = 65, value = "0.3819")
        )
      ),
      create_rln_clock_model(
        id = "anthus_nd3",
        mean_rate_prior_distr = create_uniform_distr(id = 26),
        ucldstdev_distr = create_gamma_distr(
          id = 22,
          alpha = create_alpha_param(id = 106, value = "0.5396"),
          beta = create_beta_param(id = 107, value = "0.3819")
        )
      ),
      create_strict_clock_model(
        id = "anthus_nd4",
        clock_rate_distr = create_uniform_distr(id = 24)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict strict RLN RLN", {

  expected <- c(
    "<prior id=\"ucldStdevPrior.c:anthus_nd3\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd3\">", # nolint XML
    "    <Gamma id=\"Gamma.22\" name=\"distr\">",
    "        <parameter id=\"RealParameter.106\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.107\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd3\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd3\">", # nolint XML
    "    <Uniform id=\"Uniform.26\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ClockPrior.c:anthus_nd2\" name=\"distribution\" x=\"@clockRate.c:anthus_nd2\">", # nolint XML
    "    <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_nd4\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd4\">", # nolint XML
    "    <Gamma id=\"Gamma.30\" name=\"distr\">",
    "        <parameter id=\"RealParameter.148\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.149\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd4\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd4\">", # nolint XML
    "    <Uniform id=\"Uniform.34\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>"
  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_strict_clock_model(
        id = "anthus_aco",
        clock_rate_distr = create_uniform_distr(id = "irrelevant")
      ),
      create_strict_clock_model(
        id = "anthus_nd2",
        clock_rate_distr = create_uniform_distr(id = 3)
      ),
      create_rln_clock_model(
        id = "anthus_nd3",
        mean_rate_prior_distr = create_uniform_distr(id = 26),
        ucldstdev_distr = create_gamma_distr(
          id = 22,
          alpha = create_alpha_param(id = 106, value = "0.5396"),
          beta = create_beta_param(id = 107, value = "0.3819")
        )
      ),
      create_rln_clock_model(
        id = "anthus_nd4",
        mean_rate_prior_distr = create_uniform_distr(id = 34),
        ucldstdev_distr = create_gamma_distr(
          id = 30,
          alpha = create_alpha_param(id = 148, value = "0.5396"),
          beta = create_beta_param(id = 149, value = "0.3819")
        )
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("RLN strict strict RLN", {

  expected <- c(
    "<prior id=\"ClockPrior.c:anthus_nd2\" name=\"distribution\" x=\"@clockRate.c:anthus_nd2\">", # nolint XML
    "    <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_nd4\" name=\"distribution\" x=\"@ucldStdev.c:anthus_nd4\">", # nolint XML
    "    <Gamma id=\"Gamma.30\" name=\"distr\">",
    "        <parameter id=\"RealParameter.148\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.149\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>",
    "<prior id=\"MeanRatePrior.c:anthus_nd4\" name=\"distribution\" x=\"@ucldMean.c:anthus_nd4\">", # nolint XML
    "    <Uniform id=\"Uniform.34\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ClockPrior.c:anthus_nd3\" name=\"distribution\" x=\"@clockRate.c:anthus_nd3\">", # nolint XML
    "    <Uniform id=\"Uniform.20\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_aco\" name=\"distribution\" x=\"@ucldStdev.c:anthus_aco\">", # nolint XML
    "    <Gamma id=\"Gamma.6\" name=\"distr\">",
    "        <parameter id=\"RealParameter.21\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.22\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>"
  )
  created <- beautier:::clock_models_to_xml_prior_distr(
    list(
      create_rln_clock_model(
        id = "anthus_aco",
        mean_rate_prior_distr = create_uniform_distr(id = "irrelevant"),
        ucldstdev_distr = create_gamma_distr(
          id = 6,
          alpha = create_alpha_param(id = 21, value = "0.5396"),
          beta = create_beta_param(id = 22, value = "0.3819")
        )
      ),
      create_strict_clock_model(
        id = "anthus_nd2",
        clock_rate_distr = create_uniform_distr(id = 3)
      ),
      create_strict_clock_model(
        id = "anthus_nd3",
        clock_rate_distr = create_uniform_distr(id = 20)
      ),
      create_rln_clock_model(
        id = "anthus_nd4",
        mean_rate_prior_distr = create_uniform_distr(id = 34),
        ucldstdev_distr = create_gamma_distr(
          id = 30,
          alpha = create_alpha_param(id = 148, value = "0.5396"),
          beta = create_beta_param(id = 149, value = "0.3819")
        )
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
