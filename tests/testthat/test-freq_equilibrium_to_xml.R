context("test-freq_equilibrium_to_xml")

test_that("use, estimated", {

  created <- freq_equilibrium_to_xml(
    freq_equilibrium = "estimated",
    id = "my_id"
  )
  expected <- "<frequencies id=\"estimatedFreqs.s:my_id\" spec=\"Frequencies\" frequencies=\"@freqParameter.s:my_id\"/>" # nolint XML can be long
  expect_equal(created, expected)
})

test_that("use, empirical", {

  created <- freq_equilibrium_to_xml(
    freq_equilibrium = "empirical",
    id = "an_id"
  )
  expected <- "<frequencies id=\"empiricalFreqs.s:an_id\" spec=\"Frequencies\" data=\"@an_id\"/>" # nolint XML can be long
  expect_equal(created, expected)
})

test_that("use, all_equal", {

  created <- freq_equilibrium_to_xml(
    freq_equilibrium = "all_equal",
    id = "this_id"
  )
  expected <- "<frequencies id=\"equalFreqs.s:this_id\" spec=\"Frequencies\" data=\"@this_id\" estimate=\"false\"/>" # nolint XML can be long
  expect_equal(created, expected)
})
