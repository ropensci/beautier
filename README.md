# beastscriptr

Branch|[![Travis CI logo](TravisCI.png)](https://travis-ci.org)|[![Codecov logo](Codecov.png)](https://www.codecov.io)
---|---|---
master|[![Build Status](https://travis-ci.org/richelbilderbeek/beastscriptr.svg?branch=master)](https://travis-ci.org/richelbilderbeek/beastscriptr)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beastscriptr/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/beastscriptr/branch/master)
develop|[![Build Status](https://travis-ci.org/richelbilderbeek/beastscriptr.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/beastscriptr)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beastscriptr/coverage.svg?branch=develop)](https://codecov.io/github/richelbilderbeek/beastscriptr/branch/develop)

`beastscriptr` is `BEAUti` from R.

![beastscriptr logo](beastscriptr_logo.png)

The purpose of `beastscriptr` is to create 
[a valid BEAST2 XML input file](inst/extdata/2_4.xml)
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
   * (Coalescent Bayesian Skyline)

Unlike BEAUti, `beastscriptr` *does* allow for a fixed crown age.

## Examples

See [Examples](Examples.md)

## Installation

If you use the `devtools` R package, this is easy:

```
devtools::install_github("richelbilderbeek/beastscriptr")
```

## FAQ

See [FAQ](Faq.md)

## Future use cases

See [Future](Future.md)

## There is a feature I miss

See [CONTRIBUTING](CONTRIBUTING.md), at `Submitting use cases`

## I want to collaborate

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting code'

## I think I have found a bug

See [CONTRIBUTING](CONTRIBUTING.md), at 'Submitting bugs' 

## There's something else I want to say

Sure, just add an Issue. Or send an email.

## References

Article about `beastscriptr`:

 * Bilderbeek, Richel J.C., Etienne, Rampal S., "beastscriptr: BEAUti from R" *In preparation*.

FASTA files `Anthus_aco.fas` and `Anthus_nd2.fas` from:
 
 * Van Els, Paul, and Heraldo V. Norambuena. "A revision of species limits in Neotropical pipits Anthus based on multilocus genetic and vocal data." Ibis.
