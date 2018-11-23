context("test-taxa_to_xml_tree")

test_that("use", {
  created <- taxa_to_xml_tree("my_id")
  expected <- c(
    "<tree id=\"Tree.t:my_id\" name=\"stateNode\">",
    "    <taxonset id=\"TaxonSet.my_id\" spec=\"TaxonSet\">",
    "        <alignment idref=\"my_id\"/>",
    "    </taxonset>",
    "</tree>"
  )
  expect_equal(created, expected)
})
