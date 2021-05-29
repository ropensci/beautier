test_that("v2.6, tipdates", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv"),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  # taxa_to_xml_tree calls tipdate_taxa_to_xml_tree
  created <- tipdate_taxa_to_xml_trait(
    inference_model = inference_model
  )
  # Extracted from tipdates_2_6.xml
  expected <- c(
    "<trait id=\"dateTrait.t:test_output_0\" spec=\"beast.evolution.tree.TraitSet\" traitname=\"date\" value=\"\">", # nolint
    "    <taxa id=\"TaxonSet.test_output_0\" spec=\"TaxonSet\">", # nolint
    "        <alignment idref=\"test_output_0\"/>", # nolint
    "    </taxa>", # nolint
    "</trait>" # nolint
  )
  expect_equal(created, expected)
})
