# beautier

Branch|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---
master|[![Build Status](https://travis-ci.org/richelbilderbeek/beautier.svg?branch=master)](https://travis-ci.org/richelbilderbeek/beautier)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beautier/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/beautier/branch/master)
develop|[![Build Status](https://travis-ci.org/richelbilderbeek/beautier.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/beautier)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beautier/coverage.svg?branch=develop)](https://codecov.io/github/richelbilderbeek/beautier/branch/develop)

[![Build status](https://ci.appveyor.com/api/projects/status/github/richelbilderbeek/beautier?svg=true)](https://ci.appveyor.com/api/projects/status/github/richelbilderbeek/beautier?svg=true)

`beautier` is `BEAUti` for R.

![beautier logo](pics/beautier_logo.png)

The purpose of `beautier` is to create 
[a valid BEAST2 XML input file](inst/extdata/2_4.xml)
from its function arguments. In this way, a scientific pipeline using 
`BEAST2` can be fully scripted, instead of using `BEAUti`'s GUI.

`beautier` is part of the [babette](https://github.com/richelbilderbeek/babette) package suite:

 * [beautier](https://github.com/richelbilderbeek/beautier) creates BEAST2 input (`.xml`) files.
 * [beastier](https://github.com/richelbilderbeek/beastier) runs BEAST2
 * [tracerer](https://github.com/richelbilderbeek/tracerer) pastes BEAST2 output (`.log`, `.trees`, etc) files.

## Examples

See [examples](examples.md).

## Installation

If you use the `devtools` R package, this is easy:

```
devtools::install_github("richelbilderbeek/beautier")
```

## FAQ

See [FAQ](Faq.md)

## Missing features/unsupported

`beautier` cannot do everything `BEAUti` can. 

Here are some missing or (yet) unsupported features:

 * Support for shared site or clock models (will be added in `v1.14`)
 * Support for amino acid sequences
 * Support for three or more alignments with a mix of linked site models, clock models and tree priors
 * Support for three or more MRCA priors
 * Clock models
   * Relaxed exponential
   * Random local
 * Tree priors
   * Calibrated Yule model
   * Coalescent Extended Bayesian Skyline
 * Initialization (this is a tab that is hidden by default in `BEAUti`)

## There is a feature I miss

See [CONTRIBUTING](CONTRIBUTING.md), at `Submitting use cases`

## I want to collaborate

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting code'

## I think I have found a bug

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting bugs' 

## There's something else I want to say

Sure, just add an Issue. Or send an email.

## External links

 * [BEAST2 GitHub](https://github.com/CompEvol/beast2)

## References

Article about `babette`:

 * Bilderbeek, Richel J.C., Etienne, Rampal S., "babette: BEAUti 2, BEAST2 and Tracer for R". bioRxiv 271866; doi: https://doi.org/10.1101/271866

FASTA files `anthus_aco.fas` and `anthus_nd2.fas` from:
 
 * Van Els, Paul, and Heraldo V. Norambuena. "A revision of species limits in Neotropical pipits Anthus based on multilocus genetic and vocal data." Ibis.

