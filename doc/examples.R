## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----load_beautier------------------------------------------------------------
library(beautier)

## ----show_input_file----------------------------------------------------------
input_filename <- get_beautier_path("anthus_aco_sub.fas")

## ----example_1----------------------------------------------------------------
create_beast2_input(
  input_filename
)

## ----example_2----------------------------------------------------------------
create_beast2_input(
  input_filename,
  site_model = create_jc69_site_model()
)

## ----example_3----------------------------------------------------------------
create_beast2_input(
  input_filename,
  clock_model = create_rln_clock_model()
)

## ----example_4----------------------------------------------------------------
create_beast2_input(
  input_filename,
  tree_prior = create_bd_tree_prior() 
)

## ----example_5----------------------------------------------------------------
create_beast2_input(
  input_filename,
  tree_prior = create_yule_tree_prior(
    birth_rate_distr = create_normal_distr()
  ) 
)

## ----example_6----------------------------------------------------------------
create_beast2_input(
  input_filename,
  site_model = create_hky_site_model(
    gamma_site_model = create_gamma_site_model(prop_invariant = 0.5)
  )
)

## ----example_7----------------------------------------------------------------
create_beast2_input(
  input_filename,
  clock_model = create_strict_clock_model(
    clock_rate_param = 0.5
  ) 
)

## ----example_8----------------------------------------------------------------
create_beast2_input(
  input_filename,
  mrca_prior = create_mrca_prior()
)

## ----example_9----------------------------------------------------------------
crown_age <- 15

create_beast2_input(
  input_filename,
  mrca_prior = create_mrca_prior(
    is_monophyletic = TRUE,
    mrca_distr = create_normal_distr(
      mean = crown_age,
      sigma = 0.001
    )
  )
)

