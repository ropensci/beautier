# beastscriptr

[![Build Status](https://travis-ci.org/richelbilderbeek/beastscriptr.svg?branch=master)](https://travis-ci.org/richelbilderbeek/beastscriptr)
[![codecov.io](https://codecov.io/github/richelbilderbeek/beastscriptr/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/beastscriptr?branch=master)
[![gplv3](http://www.gnu.org/graphics/gplv3-88x31.png)](http://www.gnu.org/licenses/gpl.html)

The purpose of `beastscriptr` is to create 
[a valid BEAST2 XML input file](inst/extdata/birth_death_0_20151005.xml)
from its function arguments. In this way, a scientific pipeline using 
`BEAST2` can be fully scripted, instead of using `BEAUti2` its GUI.

## Installation

If you use the `devtools` R package, this is easy:

```
devtools::install_github("richelbilderbeek/beastscriptr")
```

## Demonstration

The core function is called `beast_scriptr`:

```
beast_scriptr(
  input_fasta_filename = fasta_filename,
  mcmc_chainlength = mcmc_chainlength,
  tree_prior = tree_prior,
  date_str = date_str,
  output_xml_filename = output_xml_filename
)
```

It is demonstrated in the vignette, you can see its PDF [here](demo.pdf).

## Current limitations

`BEAUti2` contains multiple tree priors. This package currently
only supports:

 * These tree priors:
    * the constant-rate birth-death model
    * the contant-population coalescent model
 * DNA data

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
