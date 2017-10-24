# Contributing

Awesome that you are reading this!

This GitHub follows the [Contributor Covenant Code of Conduct](code_of_conduct.md).

 * For questions, you can create an Issue
 * Code changes go via Pull Requests

## Submitting use cases

Use cases within the default BEAUti environment are welcomed.

Please send all that is needed to reproduce the use case:

 * the alignment file
 * screenshots of BEAUti settings you've changed
 * the resulting XML file
 * (optional) the desired call to `beastscriptr`

BEAUti plugins are not supported (for now). See [CONTRIBUTING](CONTRIBUTING.md)
if you'd like `beastscriptr` to do so.

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

## Submitting bugs

Awesome! These are your options:

 * Add an Issue, with the test that fails
 * Submit a Pull Request, where the test is added to the `tests/testthat` folder

Pull Requests should follow the same guidelines as 'Submitting code'.

## Branching policy

 * The `master` branch should always build successfully
 * The `development` branch is for developers

## git usage

To get started working on `beastscriptr` do:

```
git clone https://github.com/richelbilderbeek/beastscriptr
```

Development is done on the `develop` branch. 
To download and checkout the `develop` branch, 
first go into the `beastscriptr` folder (`cd beastscriptr`), then do:

```
git checkout -b develop origin/develop
```

Then the workflow is the common `git` workflow:

```
git pull
git add --all :/
git commit -m "Did something awesome"
git push
```
