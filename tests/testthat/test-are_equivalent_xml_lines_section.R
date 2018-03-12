context("are_equivalent_xml_lines_section")

test_that("use", {

  lines_1 <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><beast beautitemplate='Standard' beautistatus='' namespace=\"beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood\" required=\"\" version=\"2.4\">", # nolint XML can be long
    "",
    "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"10000\">",
    "    <init id=\"RandomTree.t:anthus_aco_sub\" spec=\"beast.evolution.tree.RandomTree\" estimate=\"false\" initial=\"@Tree.t:anthus_aco_sub\" taxa=\"@anthus_aco_sub\">", # nolint XML can be long
    "        <populationModel id=\"ConstantPopulation0.t:anthus_aco_sub\" spec=\"ConstantPopulation\">", # nolint XML can be long
    "            <parameter id=\"randomPopSize.t:anthus_aco_sub\" name=\"popSize\">1.0</parameter>", # nolint XML can be long
    "        </populationModel>",
    "    </init>",
    "",
    "    <logger id=\"tracelog\" fileName=\"anthus_aco_sub.log\" logEvery=\"1000\" model=\"@posterior\" sanitiseHeaders=\"true\" sort=\"smart\">", # nolint XML can be long
    "        <log idref=\"posterior\"/>",
    "    </logger>",
    "",
    "</run>",
    "",
    "</beast>"
  )

  lines_2 <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><beast beautitemplate='Standard' beautistatus='' namespace=\"beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood\" required=\"\" version=\"2.4\">", # nolint XML can be long
    "",
    "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"10000\">",
    "    <init id=\"RandomTree.t:anthus_aco_sub\" spec=\"beast.evolution.tree.RandomTree\" estimate=\"false\" initial=\"@Tree.t:anthus_aco_sub\" taxa=\"@anthus_aco_sub\">", # nolint XML can be long
    "        <populationModel id=\"ConstantPopulation0.t:anthus_aco_sub\" spec=\"ConstantPopulation\">", # nolint XML can be long
    "            <parameter id=\"randomPopSize.t:anthus_aco_sub\" name=\"popSize\">1.0</parameter>", # nolint XML can be long
    "        </populationModel>",
    "    </init>",
    "",
    "    <logger id=\"tracelog\" fileName=\"anthus_aco_sub.log\" logEvery=\"1000\" model=\"@posterior\" sanitiseHeaders=\"true\" sort=\"smart\">", # nolint XML can be long
    "        <log idref=\"not_a_posterior_at_all\"/>", # different value
    "    </logger>",
    "",
    "</run>",
    "",
    "</beast>"
  )

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines_section(
      lines_1, lines_2, section = "init"
    )
  )
  testthat::expect_false(
    beautier:::are_equivalent_xml_lines_section(
      lines_1, lines_2, section = "logger"
    )
  )

})

test_that("abuse: section must be a word", {

  testthat::expect_error(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2, section = NA),
    "'section' must be a word"
  )

})

test_that("abuse: opening tag of lines 1 not found", {

  lines_1 <- c(
    "<a>",
    "  same",
    "</a>"
  )

  lines_2 <- lines_1

  testthat::expect_error(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2,
      section = "nonsense"),
    "Opening tag for 'section' could not be found in 'lines_1'"
  )
})

test_that("abuse: closing tag of lines 1 not found", {

  lines_1 <- c(
    "<a>",
    "  same",
    "<oops_a>"
  )

  lines_2 <- lines_1

  testthat::expect_error(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2,
      section = "a"),
    "Closing tag for 'section' could not be found in 'lines_1'"
  )
})







test_that("abuse: opening tag of lines 2 not found", {

  lines_1 <- c(
    "<a>",
    "  same",
    "</a>"
  )

  lines_2 <- c(
    "<b>",
    "  nothing",
    "</b>"
  )

  testthat::expect_error(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2,
      section = "a"),
    "Opening tag for 'section' could not be found in 'lines_2'"
  )
})

test_that("abuse: closing tag of lines 2 not found", {

  lines_1 <- c(
    "<a>",
    "  same",
    "</a>"
  )

  lines_2 <- c(
    "<a>",
    "  nothing",
    "<no_slash_a>"
  )

  testthat::expect_error(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2,
      section = "a"),
    "Closing tag for 'section' could not be found in 'lines_2'"
  )
})
