context("spell_check")

test_that("no spelling errors", {
  ignore <- c(
    "babette",
    "BD",
    "beastier",
    "BEAUti",
    "beautier",
    "Bilderbeek",
    "branchRateModel",
    "CCP",
    "CEP",
    "DensiTree",
    "extdata",
    "FASTA",
    "GTR",
    "HKY",
    "http",
    "JC",
    "MCMC",
    "monophyly",
    "MRCA",
    "nucleotides",
    "phylo",
    "phylogenies",
    "phylogeny",
    "Richel",
    "RLN",
    "screenlog",
    "stdev",
    "tracelog",
    "tracerer",
    "www"
  )
  errors <- devtools::spell_check(ignore = ignore)
  expect_equal(0, length(errors))
})
