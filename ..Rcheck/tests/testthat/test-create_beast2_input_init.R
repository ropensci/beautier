context("create_beast2_input_init")

test_that("use", {

  testthat::expect_silent(
    beautier:::create_beast2_input_init(
      ids = "test_output_0",
      initial_phylogenies = NA
    )
  )

  testthat::expect_silent(
    beautier:::create_beast2_input_init(
      ids = c("a", "b"),
      initial_phylogenies = rep(NA, 2)
    )
  )

  testthat::expect_silent(
    beautier:::create_beast2_input_init(
      ids = c("a", "b"),
      initial_phylogenies = c(ape::rcoal(3), ape::rcoal(3))
    )
  )

})

test_that("abuse", {

  testthat::expect_error(
    create_beast2_input_init(
      ids = c("a", "b"),
      initial_phylogenies = c(ape::rcoal(4))
    )
  )
})

test_that("Can specify fixed crown age", {

  input_fasta_filename <- beautier::get_fasta_filename()

  # Input file must be found
  testthat::expect_equal(file.exists(input_fasta_filename), TRUE)

  # 'c' is used to convert phylo to multiPhylo
  created_fixed <- beautier:::create_beast2_input_init(
    ids = "test_output_0",
    initial_phylogenies = c(
      beautier::fasta_to_phylo(
        input_fasta_filename, crown_age = 15
      )
    )
  )

  created_nonfixed <- beautier:::create_beast2_input_init(
    ids = "test_output_0",
    initial_phylogenies = NA
  )

  # Lines below must be absent when a starting tree is given
  # <init estimate="false" id="RandomTree.t:xxx" initial="@Tree.t:xxx" spec="beast.evolution.tree.RandomTree" taxa="@xxx"> # nolint
  # </init>
  testthat::expect_equal(1,
    length(grep(pattern = "<init id=\"RandomTree.t:.*estimate=\"false\"",
      x = created_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "<init id=\"RandomTree.t:.*estimate=\"false\"",
      x = created_fixed))
  )
})
