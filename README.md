# beautier

Branch|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---
master|[![Build Status](https://travis-ci.org/richelbilderbeek/beautier.svg?branch=master)](https://travis-ci.org/richelbilderbeek/beautier)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beautier/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/beautier/branch/master)
develop|[![Build Status](https://travis-ci.org/richelbilderbeek/beautier.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/beautier)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beautier/coverage.svg?branch=develop)](https://codecov.io/github/richelbilderbeek/beautier/branch/develop)

`beautier` is `BEAUti` for R.

![beautier logo](pics/beautier_logo.png)

The purpose of `beautier` is to create 
[a valid BEAST2 XML input file](inst/extdata/2_4.xml)
from its function arguments. In this way, a scientific pipeline using 
`BEAST2` can be fully scripted, instead of using `BEAUti`'s GUI.

For analysing the BEAST2 output files, use [RBeast](https://github.com/beast-dev/RBeast).

## Supported

The default parameters of these settings are supported:

 * Site models
   * GTR
   * HKY
   * JC69
   * TN93
 * Clock models
   * Relaxed log-normal
   * Strict
 * Priors
   * Birth-Death
   * Coalescent Bayesian Skyline
   * Coalescent Constant Population
   * Coalescent Exponential Population
   * Yule

Unlike BEAUti, `beautier` *does* allow for a fixed crown age.

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

 * Support for amino acid sequences
 * Support for three or more alignments with a mix of linked site models, clock models and tree priors
 * Specify a prior that allows a sub-clade to be monophyletic 
 * Clock models
   * Relaxed exponential
   * Random local
 * Tree priors
   * Calibrated Yule model
   * Coalescent Extended Bayesian Skyline
 * Initialization (this is tab in `BEAUti`)

## There is a feature I miss

See [CONTRIBUTING](CONTRIBUTING.md), at `Submitting use cases`

## I want to collaborate

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting code'

## I think I have found a bug

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting bugs' 

## There's something else I want to say

Sure, just add an Issue. Or send an email.

## Build status of builds that `beautier` relies on

master|develop|project
---|---|---
[![Build Status](https://travis-ci.org/rsetienne/PBD.svg?branch=master)](https://travis-ci.org/rsetienne/PBD) [![codecov.io](https://codecov.io/github/rsetienne/PBD/coverage.svg?branch=master)](https://codecov.io/github/rsetienne/PBD?branch=master) | [![Build Status](https://travis-ci.org/rsetienne/PBD.svg?branch=develop)](https://travis-ci.org/rsetienne/PBD) [![codecov.io](https://codecov.io/github/rsetienne/PBD/coverage.svg?branch=master)](https://codecov.io/github/rsetienne/PBD?branch=master) | [PBD](https://github.com/rsetienne/PBD)
[![Build Status](https://travis-ci.org/beast-dev/RBeast.svg?branch=master)](https://travis-ci.org/beast-dev/RBeast) [![codecov.io](https://codecov.io/github/beast-dev/RBeast/coverage.svg?branch=master)](https://codecov.io/github/beast-dev/RBeast?branch=master) | [![Build Status](https://travis-ci.org/beast-dev/RBeast.svg?branch=develop)](https://travis-ci.org/beast-dev/RBeast) [![codecov.io](https://codecov.io/github/beast-dev/RBeast/coverage.svg?branch=master)](https://codecov.io/github/beast-dev/RBeast?branch=master) | [RBeast](https://github.com/beast-dev/RBeast)
[![Build Status](https://travis-ci.org/richelbilderbeek/ribir.svg?branch=master)](https://travis-ci.org/richelbilderbeek/ribir) [![codecov.io](https://codecov.io/github/richelbilderbeek/ribir/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/ribir?branch=master) | [![Build Status](https://travis-ci.org/richelbilderbeek/ribir.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/ribir) [![codecov.io](https://codecov.io/github/richelbilderbeek/ribir/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/ribir?branch=master) | [ribir](https://github.com/richelbilderbeek/ribir)

## References

Article about `beautier`:

 * Bilderbeek, Richel J.C., Etienne, Rampal S., "beautier: BEAUti from R" *In preparation*.

FASTA files `anthus_aco.fas` and `anthus_nd2.fas` from:
 
 * Van Els, Paul, and Heraldo V. Norambuena. "A revision of species limits in Neotropical pipits Anthus based on multilocus genetic and vocal data." Ibis.
