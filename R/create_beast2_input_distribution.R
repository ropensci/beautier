#' Creates the distribution section of a BEAST2 XML parameter file
#' @param filename_base base of the filenames
#' @param tree_priors one or more tree priors
#' @export
create_beast2_input_distribution <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  filename_base,
  tree_priors
) {
  text <- NULL
  text <- c(text,
    "    <distribution id=\"posterior\" spec=\"util.CompoundDistribution\">")
  text <- c(text,
    "        <distribution id=\"prior\" spec=\"util.CompoundDistribution\">")

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("            <distribution id=\"BirthDeath.t:",
      filename_base, "\" spec=\"beast.evolution.speciation.",
      "BirthDeathGernhard08Model\" birthDiffRate=\"@birthRate2.t:",
      filename_base, "\" relativeDeathRate=\"@relativeDeathRate2.t:",
      filename_base, "\" tree=\"@Tree.t:", filename_base,
      "\"/>")
    )
  } else {
    testit::assert(tree_priors$name == "coalescent_constant_population")
    text <- c(text, paste0(
      "            <distribution id=\"CoalescentConstant.t:",
      filename_base, "\" spec=\"Coalescent\">"))
    text <- c(text, paste0(
      "                <populationModel id=\"ConstantPopulation.t:",
      filename_base, "\" popSize=\"@popSize.t:", filename_base,
      "\" spec=\"ConstantPopulation\"/>",
      sep = ""))
    text <- c(text, paste0(
      "                <treeIntervals id=\"TreeIntervals.t:",
      filename_base, "\" spec=\"TreeIntervals\" tree=\"@Tree.t:",
      filename_base, "\"/>"))
    text <- c(text, "            </distribution>")
  }

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("            <prior id=\"BirthRatePrior.t:",
      filename_base, "\" name=\"distribution\" x=\"@birthRate2.t:",
      filename_base, "\">"))
    text <- c(text,
      paste0(
        "                <Uniform id=\"Uniform.0\" ",
        "name=\"distr\" upper=\"1000.0\"/>"
        )
      )
    text <- c(text, "            </prior>")
    text <- c(text, paste0("            <prior id=\"DeathRatePrior.t:",
      filename_base, "\" name=\"distribution\" x=\"@relativeDeathRate2.t:",
      filename_base, "\">"))
    text <- c(text,
      "                <Uniform id=\"Uniform.01\" name=\"distr\"/>")
  } else {
    testit::assert(tree_priors$name == "coalescent_constant_population")
    text <- c(text, paste0(
      "            <prior id=\"PopSizePrior.t:", filename_base,
      "\" name=\"distribution\" x=\"@popSize.t:",
      filename_base, "\">"))
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
    filename_base, "\" spec=\"TreeLikelihood\" data=\"@", filename_base,
    "\" tree=\"@Tree.t:", filename_base, "\">"))
  text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
    filename_base, "\" spec=\"SiteModel\">"))
  text <- c(text, paste0("                    <parameter id=\"mutationRate.s:",
    filename_base,
    "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>"))
  text <- c(text, paste0("                    <parameter id=\"gammaShape.s:",
    filename_base,
    "\" estimate=\"false\" name=\"shape\">1.0</parameter>"))
  text <- c(text, paste0(
    "                    <parameter id=\"proportionInvariant.s:",
    filename_base, "\" estimate=\"false\" lower=\"0.0\" ",
    "name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>"))
  text <- c(text, paste0("                    <substModel id=\"JC69.s:",
    filename_base, "\" spec=\"JukesCantor\"/>"))
  text <- c(text, "                </siteModel>")
  text <- c(text, paste0("                <branchRateModel id=\"StrictClock.c:",
    filename_base,
    "\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">"))
  text <- c(text, paste0("                    <parameter id=\"clockRate.c:",
    filename_base,
    "\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>"))
  text <- c(text, "                </branchRateModel>")
  text <- c(text, "            </distribution>")
  text <- c(text, "        </distribution>")
  text <- c(text, "    </distribution>")
  text
}
