test_that("use", {

  expect_silent(
    create_beast2_input_data_sequences(
      get_fasta_filename(),
      beauti_options = create_beauti_options(nucleotides_uppercase = FALSE)
    )
  )

  expect_silent(
    create_beast2_input_data_sequences(
      get_fasta_filename(),
      beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
    )
  )
})

test_that("v2.4", {

  created <- create_beast2_input_data_sequences(
    input_fasta_filename = get_fasta_filename(),
    beauti_options = create_beauti_options_v2_4()
  )
  expected <- c(
    "                    <sequence id=\"seq_t1\" taxon=\"t1\" totalcount=\"4\" value=\"acttgttgcgactgcgcctg\"/>", # nolint long line indeed
    "                    <sequence id=\"seq_t2\" taxon=\"t2\" totalcount=\"4\" value=\"acttattgcgactgaggccg\"/>", # nolint long line indeed
    "                    <sequence id=\"seq_t3\" taxon=\"t3\" totalcount=\"4\" value=\"acttaatgcgaatgagcccg\"/>", # nolint long line indeed
    "                    <sequence id=\"seq_t4\" taxon=\"t4\" totalcount=\"4\" value=\"aacgacccgcgatcggggat\"/>", # nolint long line indeed
    "                    <sequence id=\"seq_t5\" taxon=\"t5\" totalcount=\"4\" value=\"acttgttgcgactgagcctg\"/>"  # nolint long line indeed
  )
  expect_equal(expected, created)
})

test_that("v2.6", {

  created <- create_beast2_input_data_sequences(
    input_fasta_filename = get_fasta_filename(),
    beauti_options = create_beauti_options_v2_6()
  )
  expected <- c(
    "        <sequence id=\"seq_t1\" spec=\"Sequence\" taxon=\"t1\" totalcount=\"4\" value=\"acttgttgcgactgcgcctg\"/>", # nolint indeed a long line
    "                            ",
    "        <sequence id=\"seq_t2\" spec=\"Sequence\" taxon=\"t2\" totalcount=\"4\" value=\"acttattgcgactgaggccg\"/>", # nolint indeed a long line
    "                            ",
    "        <sequence id=\"seq_t3\" spec=\"Sequence\" taxon=\"t3\" totalcount=\"4\" value=\"acttaatgcgaatgagcccg\"/>", # nolint indeed a long line
    "                            ",
    "        <sequence id=\"seq_t4\" spec=\"Sequence\" taxon=\"t4\" totalcount=\"4\" value=\"aacgacccgcgatcggggat\"/>", # nolint indeed a long line
    "                            ",
    "        <sequence id=\"seq_t5\" spec=\"Sequence\" taxon=\"t5\" totalcount=\"4\" value=\"acttgttgcgactgagcctg\"/>" # nolint indeed a long line
  )
  expect_equal(expected, created)
})

test_that("abuse", {

  expect_error(
    create_beast2_input_data_sequences("nonse.nse")
  )
})


test_that("sort order", {

  fasta_filename_1 <- get_beautier_path("anthus_nd2.fas")

  lines <- create_beast2_input_data_sequences(fasta_filename_1)
  expect_true(substr(lines[11], 35, 46) != "seq_bas3_nd2")
  expect_true(substr(lines[11], 35, 47) == "seq_FALK1_nd2")

})
