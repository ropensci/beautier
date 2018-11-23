context("test-create_default_xml_taxonset")

test_that("use", {
  created <- create_default_xml_taxonset("my_id")
  expected <- c(
    "<tree id=\"Tree.t:my_id\" name=\"stateNode\">",
    "    <taxonset id=\"TaxonSet.my_id\" spec=\"TaxonSet\">",
    "        <alignment idref=\"my_id\"/>",
    "    </taxonset>",
    "</tree>"
  )
  expect_equal(created, expected)
})
