## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----create_test_files, include = FALSE----------------------------------
lines_test <- readLines(
  system.file(
    "extdata", "test_output_0.fas", package = "beautier"
  )
)
lines_anthus_aco <- readLines(
  system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
)
lines_anthus_nd2 <- readLines(
  system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
)

beautier::save_text(text = lines_test, filename = "my_fasta.fas")
beautier::save_text(text = lines_test, filename = "test_output_0.fas")
beautier::save_text(text = lines_test, filename = "my_alignment.fas")
beautier::save_text(text = lines_anthus_aco, filename = "anthus_aco.fas")
beautier::save_text(text = lines_anthus_nd2, filename = "anthus_nd2.fas")

## ----load_library--------------------------------------------------------
library(beautier)

## ----example_1-----------------------------------------------------------
create_beast2_input_file(
  "test_output_0.fas",
  "my_beast.xml"
)

## ----example_2-----------------------------------------------------------
create_beast2_input_file(
  "my_fasta.fas",
  "my_beast.xml",
  fixed_crown_age = TRUE,
  initial_phylogenies = fasta_to_phylo(
    fasta_filename = "my_fasta.fas",
    crown_age = 15)
)

## ----example_3-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  site_models = create_jc69_site_model()
)

## ----example_4-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  clock_models = create_rln_clock_model()
)

## ----example_5-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  tree_priors = create_bd_tree_prior() 
)

## ----example_6-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  tree_priors = create_yule_tree_prior(
    birth_rate_distr = create_normal_distr()
  ) 
)

## ----example_7-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  site_models = create_hky_site_model(
    gamma_site_model = create_gamma_site_model(prop_invariant = 0.5)
  )
)

## ----example_8-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  clock_models = create_strict_clock_model(
    clock_rate_parameter = create_clock_rate_parameter(value = 0.5)) 
)

## ----example_9-----------------------------------------------------------
create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml"
)

## ----example_10----------------------------------------------------------
beautier::create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml",
  site_models = list(
    create_hky_site_model(), 
    create_tn93_site_model()
  )
)

