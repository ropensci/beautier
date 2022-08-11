# News

Newest versions at top.

## beautier 2.6.6 (unreleased)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * None

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## beautier 2.6.5 (2022-08-11)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * Doc does not link to absent functions
 * No tests take more than 5 seconds on CRAN infrastructure

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * Removed functions that were deprecated in v2.6.4

## beautier 2.6.4 (2022-06-05)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * There is no 'beautier' temporary folder created by the tests, not the
   examples, to comply with CRAN policy

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## beautier 2.6.3 (2022-05-18)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * None

### BUG FIXES

 * At the end of the tests, delete the 'beautier' temporary folder, thanks Brian Ripley

### DEPRECATED AND DEFUNCT

 * Filenames cannot have spaces

## beautier 2.6.2 (2021-07-24)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * `check_empty_beautier_folder` works correctly on Windows

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## beautier 2.6.1 (2021-07-23)

### NEW FEATURES

 * Added `check_empty_beautier_folder` to make sure
   no temporary files are created

### MINOR IMPROVEMENTS

 * None

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * Removed all deprecated functions and function arguments
   that warn the user in the current CRAN version (v2.6)
 * Internal functions `clock_model_to_xml_prior_distr`,
   `tipdate_taxa_to_xml_tree`, `tree_priors_to_xml_operators`
   work with `infererence_model`,
   deprecated other arguments
 * Deprecated internal function `clock_models_to_xml_prior_distr`,
   redirect user to `clock_model_to_xml_prior_distr`
 * Deprecated internal function `tree_priors_to_xml_operators`,
   redirect user to `tree_prior_to_xml_operators`
 * Deprecated `clock_models_to_xml_state`,
   redirect user to `clock_model_to_xml_state`
 * Deprecated `clock_models_to_xml_operators`,
   redirect user to `clock_model_to_xml_operators`
 * Deprecated `clock_models_to_xml_tracelog`,
   redirect user to `clock_model_to_xml_tracelog`
 * Deprecated `mrca_priors_to_xml_tracelog`,
   redirect user to `mrca_prior_to_xml_tracelog`
 * Deprecated `tree_models_to_xml_tracelog`,
   redirect user to `tree_model_to_tracelog_xml`

## beautier 2.6 (2021-05-22)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * Make sure that no files are created in `~/.cache`, fix #129

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## beautier 2.5.2 (2021-05-22)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * Removed `LazyData: true` from DESCRIPTION

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## beautier 2.5.1 (2021-05-22)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * GitHub Actions scheduler tests package once per week

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## beautier 2.5 (2021-05-14)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * Use GitHub Actions to check build
 * Check more setups, thanks @GaryNapier
 * Better synergy with `system2`

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * `phylo_to_xml_state` removed 
 * `site_models_to_xml_state` gives a deprecation message
 * `tree_priors_to_xml_state` gives a deprecation message
 * `mrca_priors_to_xml_state` has the arguments `mrca_prior`
   and `has_non_strict_clock_model` replaced by `inference_model`

## beautier 2.4 (2020-10-15)

### NEW FEATURES

 * Add `inference_models` vignette

### MINOR IMPROVEMENTS

 * Added `create_ns_inference_model`
 * No `testthat` tests in examples

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## beautier 2.3.7 (2020-08-05)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * Internal functions have better names
 * More internal functions use an `inference_model` (instead of 
   multiple elements of it)

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## beautier 2.3.6 (2020-04-21)

### NEW FEATURES

 * None

### MINOR IMPROVEMENTS

 * None

### BUG FIXES

 * Fix bug when using tipdating, thanks Katherine S. Walter, @ksw9

### DEPRECATED AND DEFUNCT

 * None

## beautier 2.3.5 (2020-02-25)

### NEW FEATURES

 * Add `get_mcmc_filenames` and `get_inference_model_filenames`
 * Add `rename_mcmc_filenames` and `rename_inference_model_filenames`
 * Add rename functions `get_remove_dir_fun`, `get_remove_hex_fun` and `get_replace_dir_fun`

### MINOR IMPROVEMENTS

 * Link to https of BEAST2

### BUG FIXES

 * None

### DEPRECATED AND DEFUNCT

 * None

## beautier 2.3.4 (2020-01-15)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Lost dependency on orphaned `geiger` package

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * Moved `fasta_to_phylo` and `fastas_to_phylos` to repository at
    `https://github.com/richelbilderbeek/ribir` to lose dependency on
    orphaned `geiger` package

## beautier 2.3.3 (2019-12-26)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Implemented all @lintr-bot's suggestions

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None

## beautier 2.3.2 (2019-11-30)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * None

### BUG FIXES

  * Remove files for CRAN submission

### DEPRECATED AND DEFUNCT

  * None

## beautier 2.3.1 (2019-11-29)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Use `testthat` in documentation, instead of the less known `testit`

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None

## beautier 2.3 (2019-10-27)

### NEW FEATURES

  * Added `create_tracelog`, `create_screenlog`, `create_treelog`, that
    indicate where BEAST2 will store its output file
  * `create_mcmc` has all elements that BEAUti has
  * Added testing functions, that use a short MCMC chain length and/or
    a simple inference model: `create_test_mcmc`, `create_test_tracelog`,
    `create_test_screenlog`, `create_test_treelog`
    and `create_test_inference_model`

### MINOR IMPROVEMENTS

  * None

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None


## beautier 2.2.4 (2019-10-15)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Added `create_beast2_input_from_model` function

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None



## beautier 2.2.3 (2019-09-10)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Added `check_mcmc_nested_sampling` function

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None

## beautier 2.2.2 (2019-09-10)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Exported and documented more `is_` functions
  * Added `check_file_exists` function
  * Added `is_one_int` and `is_one_double` functions

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None

## beautier 2.2.1 (2019-03-08)

### NEW FEATURES

  * `beautier` has passed rOpenSci peer review
  * `beautier` is on CRAN

### MINOR IMPROVEMENTS

  * Exported multiple `is_x` and `check_x` functions

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None

## beautier 1.15 (2018-11-10)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * None

### BUG FIXES

  * Correct BEAST2 files are created when using a RLN clock
    model, with an MRCA prior with a distribution, thanks @rscherrer
    and Jana Riederer

### DEPRECATED AND DEFUNCT

  * Support for multiple alignments, site models, clock models,
    tree models has decreased 

## beautier 1.14.2 (2018-10-30)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Tested to build under macOS

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None

## beautier 1.14.1 (2018-10-29)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Documented the simplified interface for parameters better
  * Use the simplified interface for parameters as defaults more often

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None

## beautier 1.14.0 (2018-10-26)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Simplified interface for parameters:

```
# Old
distr <- create_distr_poisson(id = 1, lambda = create_lambda_param(value = 1.2))
# Added
distr <- create_distr_poisson(id = 1, lambda = 1.2)
```

### BUG FIXES

  * When using MRCA priors with/without monophyly with/without
    a distribution, resulted in incorrect BEAST2 `.xml` files

### DEPRECATED AND DEFUNCT

  * Cannot set parameters to be estimated, when the resulting
    BEAST2 `.xml` would be incomplete

## beautier 1.13.4 (2018-09-11)

### NEW FEATURES

  * Support for [a Nested-Sampling MCMC run](https://github.com/BEAST2-Dev/nested-sampling)

### MINOR IMPROVEMENTS

  * None

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None

## beautier 1.13.3 (2018-05-17)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Tagged for [the academic article](https://github.com/ropensci/babette_article) about `babette`

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None

## beautier 1.13.2 (2018-04-05)

### NEW FEATURES

  * None

### MINOR IMPROVEMENTS

  * Follow all [rOpenSci packaging guidelines](https://github.com/ropensci/onboarding/blob/master/packaging_guide.md)

### BUG FIXES

  * None

### DEPRECATED AND DEFUNCT

  * None
