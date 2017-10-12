context("create_beast2_input_data")

# <?xml version="1.0" encoding="UTF-8" standalone="no"?><beast beautitemplate='Standard' beautistatus='' namespace="beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood" required="" version="2.4">
#
#
#     <data
# id="test_output_0"
# name="alignment">
#                     <sequence id="seq_t1" taxon="t1" totalcount="4" value="acttgttgcgactgcgcctg"/>
#                     <sequence id="seq_t2" taxon="t2" totalcount="4" value="acttattgcgactgaggccg"/>
#                     <sequence id="seq_t3" taxon="t3" totalcount="4" value="acttaatgcgaatgagcccg"/>
#                     <sequence id="seq_t4" taxon="t4" totalcount="4" value="aacgacccgcgatcggggat"/>
#                     <sequence id="seq_t5" taxon="t5" totalcount="4" value="acttgttgcgactgagcctg"/>
#                 </data>

test_that("sequences are sorted", {

  lines <- create_beast2_input_data_sequences(
      input_fasta_filenames = beastscriptr::get_input_fasta_filename()
    )
  expected <- "                    <sequence id=\"seq_t1\" taxon=\"t1\" totalcount=\"4\" value=\"acttgttgcgactgcgcctg\"/>"
  created <- lines[1]
  testthat::expect_equal(expected, created)
})


test_that("sequences are sorted, as per v2.4", {

  lines <- create_beast2_input_data(
      input_fasta_filenames = beastscriptr::get_input_fasta_filename()
    )
  expected <- "                    <sequence id=\"seq_t1\" taxon=\"t1\" totalcount=\"4\" value=\"acttgttgcgactgcgcctg\"/>"
  created <- lines[4]
  testthat::expect_equal(expected, created)

})

test_that("abuse", {

  testthat::expect_error(
    create_beast2_input_data(
      input_fasta_filenames = "abs.ent"
    )
  )
})
