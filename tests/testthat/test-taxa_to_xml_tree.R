test_that("v2.4", {
  check_empty_beautier_folder()

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- taxa_to_xml_tree(
    inference_model = inference_model
  )
  expected <- c(
    "<tree id=\"Tree.t:test_output_0\" name=\"stateNode\">",
    "    <taxonset id=\"TaxonSet.test_output_0\" spec=\"TaxonSet\">",
    "        <alignment idref=\"test_output_0\"/>", # nolint this is no absolute path
    "    </taxonset>",
    "</tree>"
  )
  expect_equal(created, expected)
})

test_that("v2.4, dated tips", {
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("G_VII_pre2003_msa.fas"),
    inference_model = create_test_inference_model(
      tipdates_filename = get_beautier_path("G_VII_pre2003_dates_4.txt"),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- taxa_to_xml_tree(
    inference_model = inference_model
  )
  expected <- c(
    "<tree id=\"Tree.t:G_VII_pre2003_msa\" name=\"stateNode\">",
    "    <trait id=\"dateTrait.t:G_VII_pre2003_msa\" spec=\"beast.evolution.tree.TraitSet\" traitname=\"date-forward\" value=\"KF767106_Indonesia_1976_VII=1976,KF767104_Indonesia_1988_VII=1988,KF767105_Indonesia_1988_VII=1988,AY288998_Indonesia_1990_VII=1990,JN986837_Netherlands_1993_VII-a=1993,U62620_Taiwan_1995_VII=1995,AY028995_China_1996_VII-f=1996,AF162714_China_1997_VII-e=1997,KJ782375_China_1997_VII-e=1997,MF278925.1_China_1997_VII-e=1997,EF589133_China_1998_VII-b=1998,AF364835_China_1998_VII-d=1998,AF456436_China_1998_VII-d=1998,AF456437_China_1998_VII-e=1998,AF358787_China_1999_VII-d=1999,DQ080015_China_1999_VII-d=1999,JN599167_China_1999_VII-d=1999,AB853927_Japan_1999_VII-e=1999,DQ659677_China_1999_VII-e=1999,DQ227246_China_1999_VII-f=1999,FJ754271_China_2000_VII-b=2000,JX244790_China_2000_VII-b=2000,AF358788_China_2000_VII-d=2000,AF431744_China_2000_VII-d=2000,AF456438_China_2000_VII-d=2000,EU140947_SouthKorea_2000_VII-d=2000,FJ754272_China_2000_VII-d=2000,FJ754273_China_2000_VII-d=2000,AF358786_Taiwan_2000_VII-e=2000,DQ067447_China_2000_VII-e=2000,DQ485256_China_2000_VII-e=2000,DQ485258_China_2000_VII-e=2000,AF458010_China_2000_VII-f=2000,AF456443_China_2001_VII-b=2001,DQ227247_China_2001_VII-b=2001,DQ486859_China_2001_VII-b=2001,FJ480796_China_2001_VII-b=2001,AF456442_China_2001_VII-d=2001,AF456444_China_2001_VII-d=2001,AY253912_China_2001_VII-d=2001,DQ227248_China_2001_VII-d=2001,DQ417110_China_2001_VII-d=2001,FJ608343_China_2001_VII-d=2001,EU258658_China_2002_VII-b=2002,EU258661_China_2002_VII-b=2002,EU258662_China_2002_VII-b=2002,AF473851_China_2002_VII-d=2002,DQ363535_China_2002_VII-d=2002,DQ485229_China_2002_VII-d=2002,EF579732_China_2002_VII-d=2002,FJ872531_China_2002_VII-d=2002,JF340367_China_2002_VII-d=2002,KU710278_Ukraine_2002_VII-d=2002,GU332646_Vietnam_2002_VII-e=2002,GU332647_Vietnam_2002_VII-e=2002,JX193075_China_2002_VII-e=2002,AB853329_Japan_2002_VII-e=2002,AB853929_Japan_2002_VII-e=2002\">", # nolint indeed a long line
    "        <taxa id=\"TaxonSet.G_VII_pre2003_msa\" spec=\"TaxonSet\">",
    "            <alignment idref=\"G_VII_pre2003_msa\"/>", # nolint this is no absolute path
    "        </taxa>",
    "    </trait>",
    "    <taxonset idref=\"TaxonSet.G_VII_pre2003_msa\"/>", # nolint this is no absolute path
    "</tree>"
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
  created <- taxa_to_xml_tree(
    inference_model = inference_model
  )
  expected <- c(
    "<tree id=\"Tree.t:test_output_0\" spec=\"beast.evolution.tree.Tree\" name=\"stateNode\">", # nolint indeed a long line
    "                ",
    "    <taxonset id=\"TaxonSet.test_output_0\" spec=\"TaxonSet\">",
    "                        ",
    "        <alignment idref=\"test_output_0\"/>",
    "                    ",
    "    </taxonset>",
    "            ",
    "</tree>"
  )
  expect_equal(created, expected)
})

test_that("v2.6, tipdates", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv"),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  # taxa_to_xml_tree calls tipdate_taxa_to_xml_tree
  created <- taxa_to_xml_tree(
    inference_model = inference_model
  )
  # Extracted from tipdates_2_6.xml
  expected <- c(
    "<tree id=\"Tree.t:test_output_0\" spec=\"beast.evolution.tree.Tree\" name=\"stateNode\">", # nolint
    "    <trait id=\"dateTrait.t:test_output_0\" spec=\"beast.evolution.tree.TraitSet\" traitname=\"date\" value=\"\">", # nolint
    "        <taxa id=\"TaxonSet.test_output_0\" spec=\"TaxonSet\">", # nolint
    "            <alignment idref=\"test_output_0\"/>", # nolint
    "        </taxa>", # nolint
    "    </trait>", # nolint
    "    <taxonset idref=\"TaxonSet.test_output_0\"/>", # nolint
    "</tree>"
  )
  expect_equal(created, expected)

  check_empty_beautier_folder()
})
