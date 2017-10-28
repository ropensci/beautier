context("create_beast2_input_state_tree")

test_that("two alignments with two initial trees", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beastscriptr")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beastscriptr")
  phylo1 <- fasta_to_phylo(fasta_filename_1, crown_age = 10)
  phylo2 <- fasta_to_phylo(fasta_filename_2, crown_age = 5)
  initial_phylogenies <- c(phylo1, phylo2)
  testthat::expect_silent(
    create_beast2_input_state_tree(
      ids = c("Anthus_nd2", "Anthus_aco"),
      initial_phylogenies = initial_phylogenies
    )
  )

})

test_that("two alignments use", {

  skip("WIP")
  lines <- create_beast2_input_state_tree(ids = c("Anthus_nd2", "Anthus_aco"))
  print(lines)

  testthat::expect_equal(lines[1], "    <state id=\"state\" storeEvery=\"5000\">")
  testthat::expect_equal(lines[2], "        <tree id=\"Tree.t:Anthus_nd2\" name=\"stateNode\">")
  testthat::expect_equal(lines[3], "            <taxonset id=\"TaxonSet.Anthus_nd2\" spec=\"TaxonSet\">")
  testthat::expect_equal(lines[4], "                <alignment idref=\"Anthus_nd2\"/>")
  testthat::expect_equal(lines[5], "            </taxonset>")
  testthat::expect_equal(lines[6], "        </tree>")
  testthat::expect_equal(lines[7], "        <parameter id=\"clockRate.c:Anthus_nd2\" name=\"stateNode\">1.0</parameter>")
  testthat::expect_equal(lines[8], "        <parameter id=\"birthRate.t:Anthus_nd2\" name=\"stateNode\">1.0</parameter>")
  testthat::expect_equal(lines[9], "        <tree id=\"Tree.t:Anthus_aco\" name=\"stateNode\">")
  testthat::expect_equal(lines[10], "            <taxonset id=\"TaxonSet.Anthus_aco\" spec=\"TaxonSet\">")
  testthat::expect_equal(lines[11], "                <alignment idref=\"Anthus_aco\"/>")
  testthat::expect_equal(lines[12], "            </taxonset>")
  testthat::expect_equal(lines[13], "        </tree>")
  testthat::expect_equal(lines[14], "        <parameter id=\"birthRate.t:Anthus_aco\" name=\"stateNode\">1.0</parameter>")
  testthat::expect_equal(lines[15], "    </state>")




})

