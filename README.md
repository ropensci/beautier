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

The default parameters of these settings are supported:

 * Site models
   * JC69
   * HKY
   * TN93
   * GTR
 * Clock models
   * Strict
   * Relaxed log-normal
 * Priors
   * Yule
   * Birth-Death
   * Coalescent Constant Population
   * Coalescent Bayesian Skyline

`beastscripts` can only do a small part of `BEAUti`.

Unlike BEAUti, `beastscriptr` *does* allow for a fixed crown age.

## Example #1

Using all default settings, only specify a DNA alignment.

![All default](all_default.png)

```
# Create the BEAST2 XML input file
beastscriptr::create_beast2_input_file(
  input_fasta_filename = "my_fasta.fas",
  output_xml_filename = "my_beast.xml"
)
```

All other parameters are set to their defaults as in BEAUti

## Example #2: fixed crown age

Using all default settings, only specify a DNA alignment.

```
[No screenshot, as this cannot be done in BEAUti yet]
```

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

## Example #3: JC69 site model

![JC69](jc69_2_4.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  site_models = create_jc69_site_model(), 
  output_xml_filename = "my_beast.xml"
)
```

## Example #4: Relaxed clock log normal

![Relaxed clock log normal](relaxed_clock_log_normal_2_4.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  clock_models = create_rln_clock_model(), 
  output_xml_filename = "my_beast.xml"
)
```

## Example #5: Birth-Death tree prior

![Birth-Death tree prior](birth_death_2_4.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  tree_priors = create_bd_tree_prior(), 
  output_xml_filename = "my_beast.xml"
)
```

## Example #6: HKY site model with a non-zero proportion of invariants

![HKY example](hky_prop_invariant_0_5_2_4.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  site_models = create_hky_site_model(
    proportion_invariant = 0.5
  ), 
  output_xml_filename = "my_beast.xml"
)
```

Thanks to Yacine Ben Cheheda for this use case

## Example #7: Strict clock with a known clock rate

![Strict clock with a rate of 0.5](strict_clock_rate_0_5_2_4.png)

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  clock_models = create_strict_clock_model(rate = 0.5), 
  output_xml_filename = "my_beast.xml"
)
```

Thanks to Paul van Els and Yacine Ben Cheheda for this use case.

## Future use cases

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = "my_alignment.fas",
  site_models = create_jc69_site_model(), 
  clock_models = create_strict_clock_model(), 
  tree_priors = create_yule_tree_prior(), 
  mcmc = create_mcmc(mcmc_chainlength = 1000000),
  output_xml_filename = "my_beast.xml"
)
```

Thanks to Paul van Els for this use case.

Two input FASTA files:

```
beastscriptr::create_beast2_input_file(
  input_fasta_filenames = c("nuc.fas", "mit.fas"),
  site_models = c(
    create_jc69_site_model(), 
    create_hky_site_model()
  ),
  clock_models = c(
    create_clock_model(0.1), 
    create_clock_model(0.2)
  ),
  tree_priors = c(
    create_yule_tree_prior(), 
    create_bd_tree_prior()
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

## There is a feature I miss

See [CONTRIBUTING](CONTRIBUTING.md), at `Submitting use cases`

## I want to collaborate

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting code'

## I think I have found a bug

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting bugs' 

## There's something else I want to say

Sure, just add an Issue. Or send an email.
