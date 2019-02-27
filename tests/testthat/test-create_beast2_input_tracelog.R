context("create_beast2_input_tracelog")

test_that("use", {
  created <- create_beast2_input_tracelog(ids = 1)
  expected <- c(
    "<logger id=\"tracelog\" fileName=\"1.log\" logEvery=\"1000\" model=\"@posterior\" sanitiseHeaders=\"true\" sort=\"smart\">", # nolint
    "    <log idref=\"posterior\"/>", # nolint this is no absolute path
    "    <log idref=\"likelihood\"/>", # nolint this is no absolute path
    "    <log idref=\"prior\"/>", # nolint this is no absolute path
    "    <log idref=\"treeLikelihood.1\"/>", # nolint this is no absolute path
    "    <log id=\"TreeHeight.t:1\" spec=\"beast.evolution.tree.TreeHeightLogger\" tree=\"@Tree.t:1\"/>",  # nolint this is no absolute path
    "    <log idref=\"YuleModel.t:1\"/>", # nolint this is no absolute path
    "    <log idref=\"birthRate.t:1\"/>", # nolint this is no absolute path
    "</logger>"
  )
  expect_equal(created, expected)
})
