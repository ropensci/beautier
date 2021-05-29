test_that("v2.4", {

  created <- create_beast2_input_data(
    input_filename = get_fasta_filename(),
    beauti_options = create_beauti_options_v2_4()
  )
  expected <- c(
    "    <data",
    "id=\"test_output_0\"",
    "name=\"alignment\">",
    "                    <sequence id=\"seq_t1\" taxon=\"t1\" totalcount=\"4\" value=\"acttgttgcgactgcgcctg\"/>", # nolint long line indeed
    "                    <sequence id=\"seq_t2\" taxon=\"t2\" totalcount=\"4\" value=\"acttattgcgactgaggccg\"/>", # nolint long line indeed
    "                    <sequence id=\"seq_t3\" taxon=\"t3\" totalcount=\"4\" value=\"acttaatgcgaatgagcccg\"/>", # nolint long line indeed
    "                    <sequence id=\"seq_t4\" taxon=\"t4\" totalcount=\"4\" value=\"aacgacccgcgatcggggat\"/>", # nolint long line indeed
    "                    <sequence id=\"seq_t5\" taxon=\"t5\" totalcount=\"4\" value=\"acttgttgcgactgagcctg\"/>", # nolint long line indeed
    "                </data>"
  )
  expect_equal(expected, created)
})

test_that("v2.6", {
  created <- create_beast2_input_data(
    input_filename = get_fasta_filename(),
    beauti_options = create_beauti_options_v2_6()
  )
  expected <- c(
    "    <data",
    "id=\"test_output_0\"",
    "spec=\"Alignment\"",
    "name=\"alignment\">",
    "                            ",
    "        <sequence id=\"seq_t1\" spec=\"Sequence\" taxon=\"t1\" totalcount=\"4\" value=\"acttgttgcgactgcgcctg\"/>", # nolint long line indeed
    "                            ",
    "        <sequence id=\"seq_t2\" spec=\"Sequence\" taxon=\"t2\" totalcount=\"4\" value=\"acttattgcgactgaggccg\"/>", # nolint long line indeed
    "                            ",
    "        <sequence id=\"seq_t3\" spec=\"Sequence\" taxon=\"t3\" totalcount=\"4\" value=\"acttaatgcgaatgagcccg\"/>", # nolint long line indeed
    "                            ",
    "        <sequence id=\"seq_t4\" spec=\"Sequence\" taxon=\"t4\" totalcount=\"4\" value=\"aacgacccgcgatcggggat\"/>", # nolint long line indeed
    "                            ",
    "        <sequence id=\"seq_t5\" spec=\"Sequence\" taxon=\"t5\" totalcount=\"4\" value=\"acttgttgcgactgagcctg\"/>", # nolint long line indeed
    "                        ",
    "    </data>"
  )
  expect_equal(expected, created)
})

test_that("abuse", {

  expect_error(
    create_beast2_input_data(
      input_filenames = "abs.ent"
    )
  )
})

test_that("alignment start with a capital", {

  fasta_filename <- beautier::get_beautier_path("anthus_aco.fas")

  lines <- create_beast2_input_data(
    input_filename = c(fasta_filename),
    create_beauti_options(
      capitalize_first_char_id = TRUE
    )
  )
  expect_equal(lines[2], "id=\"Anthus_aco\"")
})
