## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

tos <- c("test_output_0.fas", "my_fasta.fas", "my_alignment.fas")
for (to in tos) {
  from = beautier:::get_path("test_output_0.fas")
  file.copy(from = from, to = to)
}

file.copy(from = beautier:::get_path("anthus_aco.fas"), to = "anthus_aco.fas")
file.copy(from = beautier:::get_path("anthus_nd2.fas"), to = "anthus_nd2.fas")

## ------------------------------------------------------------------------
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
    clock_rate_param = create_clock_rate_param(value = 0.5)) 
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

## ----example_11----------------------------------------------------------
beautier::create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml",
  clock_models = list(
    create_strict_clock_model(id = "anthus_aco"), 
    create_strict_clock_model(id = "anthus_aco")
  )
)

