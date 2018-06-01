## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Create files
for (filename in c("my_fasta.fas", "my_alignment.fas")) {
  file.copy(from = beautier::get_beautier_path("anthus_aco_sub.fas"), to = filename)
}

# Copy to local
for (filename in c("test_output_0.fas", "anthus_aco.fas", "anthus_nd2.fas")) {
  file.copy(from = beautier::get_beautier_path(filename), to = filename)
}

## ------------------------------------------------------------------------
library(beautier)

## ------------------------------------------------------------------------
create_beast2_input_file(
  "test_output_0.fas",
  "my_beast.xml"
)

## ------------------------------------------------------------------------
create_beast2_input_file_1_12(
  "my_fasta.fas",
  "my_beast.xml",
  fixed_crown_age = TRUE,
  initial_phylogenies = fasta_to_phylo(
    fasta_filename = "my_fasta.fas",
    crown_age = 15)
)

## ------------------------------------------------------------------------
create_beast2_input_file(
  "my_fasta.fas",
  "my_beast.xml",
  posterior_crown_age = 15
)

## ------------------------------------------------------------------------
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
  "my_alignment.fas",
  "my_beast.xml",
  mrca_priors = create_mrca_prior(
    alignment_id = get_alignment_id("my_alignment.fas"),
    taxa_names = get_taxa_names("my_alignment.fas")
  )
)

## ----example_9_fixed_crown_age-------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  mrca_priors = create_mrca_prior(
    alignment_id = get_alignment_id("my_alignment.fas"),
    taxa_names = get_taxa_names("my_alignment.fas"),
    mrca_distr = create_normal_distr(
      mean = create_mean_param(value = 10.0),
      sigma = create_sigma_param(value = 0.01)
    )
  )
)

## ----example_10----------------------------------------------------------
crown_age <- 15
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  mrca_priors = create_mrca_prior(
    alignment_id = get_alignment_id("my_alignment.fas"),
    taxa_names = get_taxa_names("my_alignment.fas"),
    is_monophyletic = TRUE,
    mrca_distr = create_normal_distr(
      mean = create_mean_param(value = crown_age),
      sigma = create_sigma_param(value = 0.001)
    )
  )
)

## ----example_11----------------------------------------------------------
create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml"
)

## ----example_12----------------------------------------------------------
beautier::create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml",
  site_models = list(
    create_hky_site_model(), 
    create_tn93_site_model()
  )
)

## ----example_13----------------------------------------------------------
beautier::create_beast2_input_file(
  c("anthus_aco.fas", "anthus_nd2.fas"),
  "my_beast.xml",
  clock_models = list(
    create_strict_clock_model(id = "anthus_aco"), 
    create_strict_clock_model(id = "anthus_aco")
  )
)

## ----cleanup, include = FALSE--------------------------------------------
# Cleaning up
filenames <- c(
  "my_fasta.fas", 
  "my_alignment.fas",
  "test_output_0.fas", 
  "anthus_aco.fas", 
  "anthus_nd2.fas",
  "my_beast.xml"
)
for (filename in filenames) {
  if (file.exists(filename)) file.remove(filename)
}

