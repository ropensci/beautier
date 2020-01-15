# News

Newest versions at top.

## beautier 2.3.4 (unreleased)

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
