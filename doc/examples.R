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
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  site_model = create_jc69_site_model()
)

## ----example_4-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  clock_model = create_rln_clock_model()
)

## ----example_5-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  tree_prior = create_bd_tree_prior() 
)

## ----example_6-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  tree_prior = create_yule_tree_prior(
    birth_rate_distr = create_normal_distr()
  ) 
)

## ----example_7-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  site_model = create_hky_site_model(
    gamma_site_model = create_gamma_site_model(prop_invariant = 0.5)
  )
)

## ----example_8-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  clock_model = create_strict_clock_model(
    clock_rate_param = 0.5
  ) 
)

## ----example_9-----------------------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  mrca_prior = create_mrca_prior()
)

## ----example_9_fixed_crown_age-------------------------------------------
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  mrca_prior = create_mrca_prior(
    mrca_distr = create_normal_distr(
      mean = 10.0,
      sigma = 0.01
    )
  )
)

## ----example_10----------------------------------------------------------
crown_age <- 15
create_beast2_input_file(
  "my_alignment.fas",
  "my_beast.xml",
  mrca_prior = create_mrca_prior(
    is_monophyletic = TRUE,
    mrca_distr = create_normal_distr(
      mean = crown_age,
      sigma = 0.001
    )
  )
)

## ----example_11----------------------------------------------------------
# Deprecated support for now
if (1 == 2) {
  create_beast2_input_file(
    c("anthus_aco.fas", "anthus_nd2.fas"),
    "my_beast.xml"
  )
}

## ----example_13----------------------------------------------------------
# Deprecated support for now
if (1 == 2) {
  beautier::create_beast2_input_file(
    c("anthus_aco.fas", "anthus_nd2.fas"),
    "my_beast.xml",
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco"), 
      create_strict_clock_model(id = "anthus_aco")
    )
  )
}

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

