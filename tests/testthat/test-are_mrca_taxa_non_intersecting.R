context("are_mrca_taxa_non_intersecting")

test_that("use, intersection, monophyly", {

  fasta_filename <- get_beautier_path("test_output_5.fas")
  all_taxa_names <- get_taxa_names(fasta_filename)

  prior_one_two <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[1:2],
    is_monophyletic = TRUE
  )
  prior_two_three <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[2:3],
    is_monophyletic = TRUE
  )
  prior_three_four <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[3:4],
    is_monophyletic = TRUE
  )


  testthat::expect_true(
    beautier:::are_mrca_taxa_non_intersecting(
      list(prior_one_two, prior_three_four)
    )
  )
  testthat::expect_true(
    beautier:::are_mrca_taxa_non_intersecting(
      list(prior_three_four, prior_one_two)
    )
  )

  testthat::expect_false(
    beautier:::are_mrca_taxa_non_intersecting(
      list(prior_one_two, prior_two_three)
    )
  )
  testthat::expect_false(
    beautier:::are_mrca_taxa_non_intersecting(
      list(prior_two_three, prior_one_two)
    )
  )

})

test_that("use, intersection, monophyly, setdiff problem", {

  fasta_filename <- get_beautier_path("test_output_5.fas")
  all_taxa_names <- get_taxa_names(fasta_filename)

  prior_one_to_five <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[1:5],
    is_monophyletic = TRUE
  )
  prior_one_three <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[c(1, 3)],
    is_monophyletic = TRUE
  )

  non_intersecting_priors <- list(prior_one_to_five, prior_one_three)

  testthat::expect_true(
    beautier:::are_mrca_taxa_non_intersecting(
      non_intersecting_priors
    )
  )

})

test_that("use, subset", {

  fasta_filename <- get_beautier_path("test_output_5.fas")
  all_taxa_names <- get_taxa_names(fasta_filename)

  prior_one_two_three <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[1:3],
    is_monophyletic = TRUE
  )
  prior_two_three <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[2:3],
    is_monophyletic = TRUE
  )
  non_intersecting_priors <- list(prior_one_two_three, prior_two_three)

  testthat::expect_true(
    beautier:::are_mrca_taxa_non_intersecting(
      non_intersecting_priors
    )
  )

})

test_that("use, one monophyly, issue #32", {

  fasta_filename <- get_beautier_path("anthus_aco.fas")
  all_taxa_names <- get_taxa_names(fasta_filename)

  prior_one_two_three <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = c("bas3_aco", "KU3604_aco", "meridae_aco"),
    is_monophyletic = TRUE
  )
  prior_two_three <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = c(
      "KU9813_aco", "KU3604_aco", "UWBM54511_aco", "630210_aco",
      "626029_aco", "630116_aco"
    ),
    is_monophyletic = FALSE
  )
  non_intersecting_priors <- list(prior_one_two_three, prior_two_three)

  testthat::expect_false(
    beautier:::are_mrca_taxa_non_intersecting(
      non_intersecting_priors
    )
  )

})

test_that("use, zero monophylies, issue #32", {

  fasta_filename <- get_beautier_path("anthus_aco.fas")
  all_taxa_names <- get_taxa_names(fasta_filename)

  prior_one_two_three <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = c(
      "B431_aco", "61430_aco", "KU9813_aco", "B87109_aco",
      "KU25127_aco", "meridae_aco", "UWBM54556_aco",
      "KU21673_aco", "B25702_aco"
    ),
    is_monophyletic = FALSE
  )
  prior_two_three <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = c(
      "B87109_aco", "B25702_aco", "FALK1_aco", "B431_aco", "630210_aco",
      "chacoensis_aco", "meridae_aco", "UWBM54556_aco", "B48218_aco",
      "dabbenei_aco", "KU25127_aco", "B41613_aco", "KU21673_aco", "626029_aco"
    ),
    is_monophyletic = FALSE
  )
  non_intersecting_priors <- list(prior_one_two_three, prior_two_three)

  testthat::expect_false(
    beautier:::are_mrca_taxa_non_intersecting(
      non_intersecting_priors
    )
  )

})
