context("create_beast2_beautitemplate_xml")

test_that("use", {

  created <- create_beast2_beast_xml()
  expected <- paste0(
    "<beast beautitemplate='Standard' beautistatus='' ",
    "namespace=\"beast.core:beast.evolution.alignment:",
    "beast.evolution.tree.coalescent:beast.core.util:",
    "beast.evolution.nuc:beast.evolution.operators:",
    "beast.evolution.sitemodel:",
    "beast.evolution.substitutionmodel:",
    "beast.evolution.likelihood\" ",
    "required=\"\" version=\"2.4\">"
  )
  expect_equal(created, expected)
})
