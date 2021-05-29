test_that("v2.4", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model()
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  # Copied from 2_4.xml
  expected <- c(
    "<state id=\"state\" storeEvery=\"5000\">",
    "    <tree id=\"Tree.t:test_output_0\" name=\"stateNode\">",
    "        <taxonset id=\"TaxonSet.test_output_0\" spec=\"TaxonSet\">",
    "            <alignment idref=\"test_output_0\"/>",
    "        </taxonset>",
    "    </tree>",
    "    <parameter id=\"birthRate.t:test_output_0\" name=\"stateNode\">1.0</parameter>", # nolint long line indeed
    "</state>"
  )
  expect_equal(created, expected)
})

test_that("v2.6", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  # Copied from 2_6_2.xml
  expected <- c(
    "<state id=\"state\" spec=\"State\" storeEvery=\"5000\">", # nolint
    "    <tree id=\"Tree.t:test_output_0\" spec=\"beast.evolution.tree.Tree\" name=\"stateNode\">", # nolint
    "        <taxonset id=\"TaxonSet.test_output_0\" spec=\"TaxonSet\">", # nolint
    "            <alignment idref=\"test_output_0\"/>", # nolint
    "        </taxonset>", # nolint
    "    </tree>", # nolint
    "    <parameter id=\"birthRate.t:test_output_0\" spec=\"parameter.RealParameter\" name=\"stateNode\">1.0</parameter>", # nolint
    "</state>"
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("tipdates, v2.6", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv"),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  # Copied from tipdates_2_6.xml
  expected <- c(
    "<state id=\"state\" spec=\"State\" storeEvery=\"5000\">", # nolint
    "    <tree id=\"Tree.t:test_output_0\" spec=\"beast.evolution.tree.Tree\" name=\"stateNode\">", # nolint
    "        <trait id=\"dateTrait.t:test_output_0\" spec=\"beast.evolution.tree.TraitSet\" traitname=\"date\" value=\"\">", # nolint
    "            <taxa id=\"TaxonSet.test_output_0\" spec=\"TaxonSet\">", # nolint
    "                <alignment idref=\"test_output_0\"/>", # nolint
    "            </taxa>", # nolint
    "        </trait>", # nolint
    "        <taxonset idref=\"TaxonSet.test_output_0\"/>", # nolint
    "    </tree>", # nolint
    "    <parameter id=\"birthRate.t:test_output_0\" spec=\"parameter.RealParameter\" name=\"stateNode\">1.0</parameter>", # nolint
    "    <parameter id=\"clockRate.c:test_output_0\" spec=\"parameter.RealParameter\" name=\"stateNode\">1.0</parameter>", # nolint
    "</state>"
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
