test_that("minimal use", {
  created <- get_default_beast_namespace()
  expected <- get_default_beast_namespace_v2_4()
  expect_equal(created, expected)
})

test_that("minimal use", {
  created <- get_default_beast_namespace_v2_4()
  expected <- "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood" # nolint indeed a long line
  expect_equal(created, expected)
})

test_that("minimal use", {
  created <- get_default_beast_namespace_v2_6()
  expected <- "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.base.evolution.alignment:beast.pkgmgmt:beast.base.core:beast.base.inference:beast.base.evolution.tree.coalescent:beast.pkgmgmt:beast.base.core:beast.base.inference.util:beast.evolution.nuc:beast.base.evolution.operator:beast.base.inference.operator:beast.base.evolution.sitemodel:beast.base.evolution.substitutionmodel:beast.base.evolution.likelihood" # nolint indeed a long line
  expect_equal(created, expected)
})
