test_that("v2.4", {
  created <- create_beast2_input_map(
    beauti_options = create_beauti_options_v2_4()
  )
  expected <- c(
    "<map name=\"Uniform\" >beast.math.distributions.Uniform</map>",
    "<map name=\"Exponential\" >beast.math.distributions.Exponential</map>",
    "<map name=\"LogNormal\" >beast.math.distributions.LogNormalDistributionModel</map>", # nolint long line indeed
    "<map name=\"Normal\" >beast.math.distributions.Normal</map>",
    "<map name=\"Beta\" >beast.math.distributions.Beta</map>",
    "<map name=\"Gamma\" >beast.math.distributions.Gamma</map>",
    "<map name=\"LaplaceDistribution\" >beast.math.distributions.LaplaceDistribution</map>", # nolint long line indeed
    "<map name=\"prior\" >beast.math.distributions.Prior</map>",
    "<map name=\"InverseGamma\" >beast.math.distributions.InverseGamma</map>",
    "<map name=\"OneOnX\" >beast.math.distributions.OneOnX</map>"
  )
  expect_equal(created, expected)
})

test_that("v2.6", {
  created <- create_beast2_input_map(
    beauti_options = create_beauti_options_v2_6()
  )
  expected <- c(
    "<map name=\"Uniform\" >beast.math.distributions.Uniform</map>",
    "",
    "<map name=\"Exponential\" >beast.math.distributions.Exponential</map>",
    "",
    "<map name=\"LogNormal\" >beast.math.distributions.LogNormalDistributionModel</map>", # nolint long line indeed
    "",
    "<map name=\"Normal\" >beast.math.distributions.Normal</map>",
    "",
    "<map name=\"Beta\" >beast.math.distributions.Beta</map>",
    "",
    "<map name=\"Gamma\" >beast.math.distributions.Gamma</map>",
    "",
    "<map name=\"LaplaceDistribution\" >beast.math.distributions.LaplaceDistribution</map>", # nolint long line indeed
    "",
    "<map name=\"prior\" >beast.math.distributions.Prior</map>",
    "",
    "<map name=\"InverseGamma\" >beast.math.distributions.InverseGamma</map>",
    "",
    "<map name=\"OneOnX\" >beast.math.distributions.OneOnX</map>",
    ""
  )
  expect_equal(created, expected)
})
