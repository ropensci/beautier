# beastscriptr

Branch|[![Travis CI logo](TravisCI.png)](https://travis-ci.org)|[![Codecov logo](Codecov.png)](https://www.codecov.io)
---|---|---
master|[![Build Status](https://travis-ci.org/richelbilderbeek/beastscriptr.svg?branch=master)](https://travis-ci.org/richelbilderbeek/beastscriptr)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beastscriptr/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/beastscriptr/branch/master)
develop|[![Build Status](https://travis-ci.org/richelbilderbeek/beastscriptr.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/beastscriptr)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beastscriptr/coverage.svg?branch=develop)](https://codecov.io/github/richelbilderbeek/beastscriptr/branch/develop)

`beastscriptr` is `BEAUti` from R.

![beastscriptr logo](beastscriptr_logo.png)

The purpose of `beastscriptr` is to create 
[a valid BEAST2 XML input file](inst/extdata/birth_death_0_20151005.xml)
from its function arguments. In this way, a scientific pipeline using 
`BEAST2` can be fully scripted, instead of using `BEAUti` its GUI.

For analysing the BEAST2 output files, use [RBeast](https://github.com/beast-dev/RBeast).

## Supported

Site models:

 * JC69
 * HKY
 * TN93
 * GTR

Priors:

 * Yule
 * Birth-Death
 * Coalescent Constant Population
 * Coalescent Bayesian Skyline


## Example #1

```
# Create the BEAST2 XML input file
beastscriptr::create_beast2_input_file(
  input_fasta_filename = "my_fasta.fas",
  output_xml_filename = "my_beast.xml"
)
```

All other parameters are set to their defaults as in BEAUti

## Example #2: fixed crown age

```
beastscriptr::create_beast2_input_file(
  input_fasta_filename = "my_fasta.fas",
  output_xml_filename = "my_beast.xml",
  fixed_crown_age = TRUE,
  initial_phylogeny = beastscriptr::fasta_to_phylo(
    fasta_filename = "my_fasta.fas",
    crown_age = 15)
)
```

## Example #3: HKY site model

Thanks to Yacine Ben Cheheda

![HKY example](hky_example.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  site_models = create_hky_site_model(
    gamma_cat_count = 1, 
    prop_invariant = 0.2, 
    kappa = 3.0
  ), 
  output_xml_filename = "my_beast.xml"
)
```

## Future use cases

Thanks to Paul van Els.

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  site_models = create_site_model(name = "J69"), 
  clock_models = create_clock_model(name = "strict"), 
  tree_priors = create_tree_prior(name = "yule"), 
  mcmc = create_mcmc(mcmc_chainlength = 1000000),
  output_xml_filename = "my_beast.xml"
)
```

Two input FASTA files:

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = c("nuc.fas", "mit.fas"),
  site_models = c(
    create_site_model(name = "J69"), 
    create_site_model(name = "HKY")
  ),
  clock_models = c(
    create_clock_model(0.1), 
    create_clock_model(0.2)
  ),
  tree_priors = c(
    create_tree_prior(name = "yule"), 
    create_tree_prior(name = "birth_death")
  ),
  mcmc = create_mcmc(mcmc_chainlength = 1000000),
  output_xml_filename = "my_beast.xml"
)
```

## Installation

If you use the `devtools` R package, this is easy:

```
devtools::install_github("richelbilderbeek/beastscriptr")
```

## Currently supports

`beastscripts` can do only do a small part of `BEAUti`.
On the other hand, it *does* allow for a fixed crown age.

 * DNA data
 * Tree priors:
    * the constant-rate birth-death model
    * the contant-population coalescent model
 * Fixed crown age yes/no

## I want to collaborate

Great! These are your options:

 * Add an Issue
 * Submit a Pull Request

Pull Requests should
 * try to follow the [R-CodingStandard](https://github.com/richelbilderbeek/R-CodingStandard) guidelines
 * keep the package to be built without warnings and/or notes
 * not trigger any warning by `lintr`

## I think I have found a bug

Awesome! These are your options:

 * Add an Issue, with the test that fails
 * Submit a Pull Request, where the test is added to the `tests/testthat` folder

Pull Requests should
 * try to follow the [R-CodingStandard](https://github.com/richelbilderbeek/R-CodingStandard) guidelines
 * keep the package to be built without warnings and/or notes
 * not trigger any warning by `lintr`

## There's something else I want to say

Sure, just add an Issue. Or send an email.
