test_that("use, v2.6.2", {

  beauti_options <- create_beauti_options_v2_6()
  created <- create_beast2_beast_xml(
    beauti_options
  )
  expected <- paste0(
    "<beast ",
    "beautitemplate='Standard' ",
    "beautistatus='' ",
    "namespace=\"beast.core:beast.evolution.alignment:",
      "beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:",
      "beast.evolution.operators:beast.evolution.sitemodel:",
      "beast.evolution.substitutionmodel:beast.evolution.likelihood\" ",
    "required=\"\" ",
    "version=\"2.6\">"
  )
  expect_equal(created, expected)
})

test_that("use, v2.6, with status", {

  beauti_options <- create_beauti_options_v2_6(status = "noAutoSetClockRate")
  created <- create_beast2_beast_xml(
    beauti_options
  )
  expected <- paste0(
    "<beast ",
    "beautitemplate='Standard' ",
    "beautistatus='noAutoSetClockRate' ",
    "namespace=\"beast.core:beast.evolution.alignment:",
    "beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:",
    "beast.evolution.operators:beast.evolution.sitemodel:",
    "beast.evolution.substitutionmodel:beast.evolution.likelihood\" ",
    "required=\"\" ",
    "version=\"2.6\">"
  )
  expect_equal(created, expected)
})
