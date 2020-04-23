# beautier

[![Peer Review Status](https://badges.ropensci.org/209_status.svg)](https://github.com/ropensci/onboarding/issues/209)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/beautier)](https://cran.r-project.org/package=beautier)
[![](http://cranlogs.r-pkg.org/badges/grand-total/beautier)]( https://CRAN.R-project.org/package=beautier)
[![](http://cranlogs.r-pkg.org/badges/beautier)](https://CRAN.R-project.org/package=beautier)
[![DOI](https://zenodo.org/badge/53443354.svg)](https://zenodo.org/badge/latestdoi/53443354)

Branch   |[![Travis CI logo](man/figures/TravisCI.png)](https://travis-ci.org)|[![AppVeyor logo](man/figures/AppVeyor.png)](https://www.appveyor.com)|[![Codecov logo](man/figures/Codecov.png)](https://www.codecov.io)
---------|----------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------
`master` |[![Build Status](https://travis-ci.org/ropensci/beautier.svg?branch=master)](https://travis-ci.org/ropensci/beautier) | [![Build status](https://ci.appveyor.com/api/projects/status/blvjo5pulbkqxrhb/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/beautier-on-windows/branch/master) |[![codecov.io](https://codecov.io/github/ropensci/beautier/coverage.svg?branch=master)](https://codecov.io/github/ropensci/beautier/branch/master)
`develop`|[![Build Status](https://travis-ci.org/ropensci/beautier.svg?branch=develop)](https://travis-ci.org/ropensci/beautier)| None                                                                                                                                                                                       |[![codecov.io](https://codecov.io/github/ropensci/beautier/coverage.svg?branch=develop)](https://codecov.io/github/ropensci/beautier/branch/develop)

`beautier` is `BEAUti` for R.

![beautier logo](man/figures/beautier_logo.png)

The purpose of `beautier` is to create 
[a valid BEAST2 XML input file](inst/extdata/2_4.xml)
from its function arguments. In this way, a scientific pipeline using 
`BEAST2` can be fully scripted, instead of using `BEAUti`'s GUI.

`beautier` is part of the [`babette`](https://github.com/ropensci/babette) package suite:

 * [`beautier`](https://github.com/ropensci/beautier) creates BEAST2 input (`.xml`) files.
 * [`beastier`](https://github.com/ropensci/beastier) runs BEAST2
 * [`tracerer`](https://github.com/ropensci/tracerer) pastes BEAST2 output (`.log`, `.trees`, etc) files.
 * [`mauricer`](https://github.com/ropensci/mauricer) install BEAST2 packages

Related R packages:

 * [`beautier_on_windows`](https://github.com/richelbilderbeek/beautier_on_windows): verifies
   `beautier` builds on Windows
 * [`lumier`](https://github.com/ropensci/lumier): Shiny app to help create the function call needed

## Examples

See [examples](doc/examples.md).

## Installation

`beautier` can be installed:

 * Latest CRAN version: CRAN
 * Latest stable version: GitHub, `master` branch
 * Bleeding-edge version: GitHub, `develop` branch

### CRAN

For the latest CRAN version:

```r
install.packages("beautier")
```

### GitHub, `master` branch

For the latest stable version: 

```r
remotes::install_github("ropensci/beautier")
```

### GitHub, `develop` branch

For the bleeding-edge version: 

```r
remotes::install_github("ropensci/beautier", ref = "develop")
```

## [FAQ](doc/faq.md)

See [FAQ](doc/faq.md).

## Supported

This works, and the interface is unlikely to change.

 * 1 DNA alignment
 * Site models:
    * JC69
    * HKY
    * TN93
    * GTR
 * Clock models:
    * Strickt
    * Relaxed log-normal
 * Tree models:
    * Yule
    * Birth-Death
    * Coalescent Bayesian Skyline 
    * Coalescent Constant Population
    * Coalescent Exponential Population

## Experimental

This works, but the interface may change.

 * Tip dating

## Missing features/unsupported

`beautier` cannot do everything `BEAUti` can. 

Here are some missing or (yet) unsupported features:

 * Two or more DNA alignments
 * Two or more site, clock or tree models
 * Two or more MRCA priors
 * Shared site, clock and/or tree models
 * Using an amino acid alignment
 * Support for hyper parameters
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

 * Bilderbeek, Richèl JC, and Rampal S. Etienne. "`babette`: BEAUti 2, BEAST 2 and Tracer for R." Methods in Ecology and Evolution (2018). https://doi.org/10.1111/2041-210X.13032

FASTA files `anthus_aco.fas` and `anthus_nd2.fas` from:
 
 * Van Els, Paul, and Heraldo V. Norambuena. "A revision of species limits in Neotropical pipits Anthus based on multilocus genetic and vocal data." Ibis.

FASTA file `G_VII_pre2003_msa.fas` from:

 * Durr, PA; Wibowo, MH; Tabbu, CR; Asmara, W; Selleck, P; Wang, J; Broz, I; Graham, K.; Dimitrov, K and Afonso, C. (in preparation). Phylodynamics of Genotype VII Newcastle disease virus in Indonesia. 


[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)

