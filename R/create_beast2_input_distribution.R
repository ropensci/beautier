#' Creates the distribution section of a BEAST2 XML parameter file
#' @param fasta_filenames FASTA filenames
#' @param tree_priors one or more tree priors
#' @export
create_beast2_input_distribution <- function(
  fasta_filenames,
  tree_priors
) {
  ids <- beastscriptr::get_file_base_sans_ext(fasta_filenames)
  text <- NULL
  text <- c(text,
    "    <distribution id=\"posterior\" spec=\"util.CompoundDistribution\">")
  text <- c(text,
    "        <distribution id=\"prior\" spec=\"util.CompoundDistribution\">")

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("            <distribution id=\"BirthDeath.t:", ids, "\" spec=\"beast.evolution.speciation.BirthDeathGernhard08Model\" birthDiffRate=\"@BDBirthRate.t:test_output_0\" relativeDeathRate=\"@BDDeathRate.t:test_output_0\" tree=\"@Tree.t:test_output_0\"/>"))
    text <- c(text, paste0("            <prior id=\"BirthRatePrior.t:", ids, "\" name=\"distribution\" x=\"@BDBirthRate.t:", ids, "\">"))
    text <- c(text, paste0("                <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"1000.0\"/>"))

    # text <- c(text, paste0("            <distribution id=\"BirthDeath.t:",
    #   ids, "\" spec=\"beast.evolution.speciation.",
    #   "BirthDeathGernhard08Model\" birthDiffRate=\"@birthRate2.t:",
    #   ids, "\" relativeDeathRate=\"@relativeDeathRate2.t:",
    #   ids, "\" tree=\"@Tree.t:", ids,
    #   "\"/>")
    # )
  } else {
    testit::assert(tree_priors$name == "coalescent_constant_population")
    text <- c(text, paste0(
      "            <distribution id=\"CoalescentConstant.t:",
      ids, "\" spec=\"Coalescent\">"))
    text <- c(text, paste0(
      "                <populationModel id=\"ConstantPopulation.t:",
      ids, "\" popSize=\"@popSize.t:", ids,
      "\" spec=\"ConstantPopulation\"/>",
      sep = ""))
    text <- c(text, paste0(
      "                <treeIntervals id=\"TreeIntervals.t:",
      ids, "\" spec=\"TreeIntervals\" tree=\"@Tree.t:",
      ids, "\"/>"))
    text <- c(text, "            </distribution>")
  }

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("            <prior id=\"BirthRatePrior.t:",
      ids, "\" name=\"distribution\" x=\"@birthRate2.t:",
      ids, "\">"))
    text <- c(text,
      paste0(
        "                <Uniform id=\"Uniform.0\" ",
        "name=\"distr\" upper=\"1000.0\"/>"
        )
      )
    text <- c(text, "            </prior>")
    text <- c(text, paste0("            <prior id=\"DeathRatePrior.t:",
      ids, "\" name=\"distribution\" x=\"@relativeDeathRate2.t:",
      ids, "\">"))
    text <- c(text,
      "                <Uniform id=\"Uniform.01\" name=\"distr\"/>")
  } else {
    testit::assert(tree_priors$name == "coalescent_constant_population")
    text <- c(text, paste0(
      "            <prior id=\"PopSizePrior.t:", ids,
      "\" name=\"distribution\" x=\"@popSize.t:",
      ids, "\">"))
    text <- c(text, "                <OneOnX id=\"OneOnX.0\" name=\"distr\"/>")
  }

  text <- c(text, "            </prior>")
  text <- c(text, "        </distribution>")
  text <- c(
      text,
      paste0(
        "        <distribution id=\"likelihood\" ",
        "spec=\"util.CompoundDistribution\">"
      )
    )
  text <- c(text, paste0("            <distribution id=\"treeLikelihood.",
    ids, "\" spec=\"TreeLikelihood\" data=\"@", ids,
    "\" tree=\"@Tree.t:", ids, "\">"))
  text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
    ids, "\" spec=\"SiteModel\">"))
  text <- c(text, paste0("                    <parameter id=\"mutationRate.s:",
    ids,
    "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>"))
  text <- c(text, paste0("                    <parameter id=\"gammaShape.s:",
    ids,
    "\" estimate=\"false\" name=\"shape\">1.0</parameter>"))
  text <- c(text, paste0(
    "                    <parameter id=\"proportionInvariant.s:",
    ids, "\" estimate=\"false\" lower=\"0.0\" ",
    "name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>"))
  text <- c(text, paste0("                    <substModel id=\"JC69.s:",
    ids, "\" spec=\"JukesCantor\"/>"))
  text <- c(text, "                </siteModel>")
  text <- c(text, paste0("                <branchRateModel id=\"StrictClock.c:",
    ids,
    "\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">"))
  text <- c(text, paste0("                    <parameter id=\"clockRate.c:",
    ids,
    "\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>"))
  text <- c(text, "                </branchRateModel>")
  text <- c(text, "            </distribution>")
  text <- c(text, "        </distribution>")
  text <- c(text, "    </distribution>")
  text
}
