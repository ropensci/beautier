# Contributing

Awesome that you are reading this.

This GitHub follows the [Contributor Covenant Code of Conduct](doc/code_of_conduct.md).

 * For questions, you can create an Issue
 * Code changes go via Pull Requests

## Which package to contribute to?

`beautier` is part of the `babette` package suite,
which consists out of five packages.
Here is how to determine which package is best suited for your contribution:

If you want to contribute to how BEAST2 is run,
go to [beautier](https://github.com/ropensci/beautier/blob/master/CONTRIBUTING.md).

If you want to contribute to how BEAST2 output is parsed,
go to [tracerer](https://github.com/ropensci/tracerer/blob/master/CONTRIBUTING.md)

If you want to contribute regarding the BEAST2 package management,
go to [mauricer](https://github.com/ropensci/mauricer/blob/master/CONTRIBUTING.md)

If you want to contribute with an overarching idea,
go to [babette](https://github.com/ropensci/babette/blob/master/CONTRIBUTING.md).

If you want to contribute to the creation of BEAST2 XML input files, 
you are at the right spot :-) 

## Submitting use cases

Use cases within the default BEAUti environment are welcomed.

Please send all that is needed to reproduce the use case:

 * the alignment file
 * screenshots of BEAUti settings you've changed
 * the resulting XML file
 * (optional) the desired call to `beautier`

BEAUti plugins are not supported (for now). See 'Submitting code'
if you'd like `beautier` to do so.

You can do so by:

 * Add an Issue
 * Send @richelbilderbeek an email (@richelbilderbeek will make an Issue of it)

## Submitting code

Submitted code should follow these quality guidelines:

 * All tests pass cleanly/silently
 * Code coverage above 95%
 * Coding style should follow the default style by `lintr`

These are all checked by Travis CI when submitting
a Pull Request. 

For BEAUti plugins, there needs to be added:

 * Enough documentation so an example use case can be reproduced
 * Stating that maintenance will be done by the Collaborator

Emails with code will not be accepted.

## Submitting bugs

Awesome. These are your options:

 * Add an Issue, with the test that fails
 * Submit a Pull Request, where the test is added to the `tests/testthat` folder
 * Send @richelbilderbeek an email (@richelbilderbeek will make an Issue of it)

Pull Requests should follow the same guidelines as 'Submitting code'.

## Branching policy

 * The `master` branch should always build successfully
 * The `development` branch is for developers

## git usage

To get started working on `beautier` do:

```
git clone https://github.com/ropensci/beautier
```

Development is done on the `develop` branch. 
To download and checkout the `develop` branch, 
first go into the `beautier` folder (`cd beautier`), then do:

```
# First time:
git checkout -t origin/develop

# From then on:
git checkout develop
```

Then the workflow is the common `git` workflow:

```
git pull
git add --all :/
git commit -m "Did something awesome"
git push
```
