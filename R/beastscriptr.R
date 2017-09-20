#' Create a BEAST2 XML parameter file
#' @param input_fasta_filename Filename of a fasta file
#' @param mcmc_chainlength Length of MCMC chain
#' @param tree_prior The tree prior, can be 'birth_death' or
#'   'coalescent_constant_population'
#' @param output_xml_filename Filename of the XML parameter file created
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @param initial_phylogeny the MCMC chain its initial phylogeny. If
#'   this is set to NA, BEAST2 will use a random phylogeny. Else
#'   a phylogeny must be supplied of class ape::phylo.
#' @export
beast_scriptr <- function(
  input_fasta_filename,
  mcmc_chainlength,
  tree_prior,
  output_xml_filename,
  fixed_crown_age = FALSE,
  initial_phylogeny = NA
) {
  if (!file.exists(input_fasta_filename)) {
    stop("input_fasta_filename not found")
  }
  if (tree_prior != "birth_death" &&
      tree_prior != "coalescent_constant_population") {
    stop("tree_prior is not recognized")
  }
  if (mcmc_chainlength <= 0) {
    stop("mcmc_chainlength must be positive")
  }
  if (!is.logical(fixed_crown_age)) {
    stop("fixed_crown_age must be either TRUE or FALSE")
  }
  #if (fixed_crown_age == TRUE && !ribir::is_phylogeny(initial_phylogeny)) {
  #  warning("Using a fixed crown age of a random phylogeny")
  #}

  # Make a million show as 1000000 instead of 1e+06
  options(scipen = 20)

  text <- NULL
  text <- c(text, beast_scriptr_xml())
  text <- c(text, "")
  text <- c(text, "")
  filename_base <- beastscriptr::remove_file_extension(input_fasta_filename)

  text <- c(text,
    beast_scriptr_data(
      filename_base = filename_base,
      input_fasta_filename = input_fasta_filename
    )
  )

  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")

  text <- c(text, beast_scriptr_map())

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text, paste("<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"",
    mcmc_chainlength, "\">", sep = ""))

  text <- c(text,
    beast_scriptr_state(
      filename_base = filename_base,
      tree_prior = tree_prior,
      initial_phylogeny = initial_phylogeny
    )
  )

  text <- c(text, "")
  if (ribir::is_phylogeny(initial_phylogeny)) {
    text <- c(text, paste0("    <statenode spec=\"beast.util.TreeParser\" ",
      "id=\"Tree.t:", filename_base, "\" IsLabelledNewick=\"true\" ",
      "adjustTipHeights=\"false\" taxa=\"@", filename_base, "\" ",
      "newick=\"", ape::write.tree(initial_phylogeny), "\">"))
    text <- c(text, paste0("    </statenode>"))
  }

  text <- c(text, "")

  text <- c(text,
    beast_scriptr_init(
      filename_base = filename_base,
      initial_phylogeny = initial_phylogeny
    )
  )

  text <- c(text, "")

  text <- c(text,
    beast_scriptr_distribution(
      filename_base = filename_base,
      tree_prior = tree_prior
    )
  )

  text <- c(text, "")

  text <- c(text, beast_scriptr_operators(
    filename_base = filename_base,
    tree_prior = tree_prior,
    fixed_crown_age = fixed_crown_age))

  text <- c(text, "")

  text <- c(text, beast_scriptr_loggers(
    filename_base = filename_base,
    tree_prior = tree_prior))

  text <- c(text, "")
  text <- c(text, "</run>")
  text <- c(text, "")
  text <- c(text, "</beast>")

  # Write to file
  my_file <- file(output_xml_filename)
  writeLines(text, my_file)
  close(my_file)
}



#' Creates the operators section of a BEAST2 XML parameter file
#' @param filename_base base of the filenames
#' @param tree_prior The tree prior, can be 'birth_death' or
#'   'coalescent_constant_population'
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @export
beast_scriptr_operators <- function(
  filename_base,
  tree_prior,
  fixed_crown_age
) {

  text <- NULL
  text <- c(text, paste("    <operator id=\"treeScaler.t:", filename_base,
    "\" spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:",
    filename_base, "\" weight=\"3.0\"/>", sep = ""))                            # nolint (as this is no absolute path)
  text <- c(text, "")
  text <- c(text, paste("    <operator id=\"treeRootScaler.t:", filename_base,
    "\" spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" ",
    "tree=\"@Tree.t:", filename_base, "\" weight=\"3.0\"/>", sep = ""))          # nolint (as this is no absolute path)
  text <- c(text, "")
  text <- c(text, paste("    <operator id=\"UniformOperator.t:", filename_base,
    "\" spec=\"Uniform\" tree=\"@Tree.t:", filename_base,
    "\" weight=\"30.0\"/>", sep = ""))                                          # nolint (as this is no absolute path)
  text <- c(text, "")
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste("    <operator id=\"SubtreeSlide.t:", filename_base,
      "\" spec=\"SubtreeSlide\" tree=\"@Tree.t:", filename_base,
      "\" weight=\"15.0\"/>", sep = ""))                                          # nolint (as this is no absolute path)
    text <- c(text, "")
    text <- c(text, paste("    <operator id=\"narrow.t:", filename_base,
      "\" spec=\"Exchange\" tree=\"@Tree.t:", filename_base,
      "\" weight=\"15.0\"/>", sep = ""))                                          # nolint (as this is no absolute path)
    text <- c(text, "")
    text <- c(text, paste("    <operator id=\"wide.t:", filename_base,
      "\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:", filename_base,
      "\" weight=\"3.0\"/>", sep = ""))                                           # nolint (as this is no absolute path)
    text <- c(text, "")
    text <- c(text, paste("    <operator id=\"WilsonBalding.t:", filename_base,
      "\" spec=\"WilsonBalding\" tree=\"@Tree.t:", filename_base,
      "\" weight=\"3.0\"/>", sep = ""))                                           # nolint (as this is no absolute path)
    text <- c(text, "")
  }

  if (tree_prior == "birth_death") {
    text <- c(text, paste("    <operator id=\"BirthRateScaler.t:",
      filename_base, "\" spec=\"ScaleOperator\" parameter=\"@birthRate2.t:",
      filename_base, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>", sep = ""))     # nolint (as this is no absolute path)
    text <- c(text, "")
    text <- c(text, paste("    <operator id=\"DeathRateScaler.t:",
      filename_base,
      "\" spec=\"ScaleOperator\" parameter=\"@relativeDeathRate2.t:",
      filename_base, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>", sep = ""))     # nolint (as this is no absolute path)
  } else {
    testit::assert(tree_prior == "coalescent_constant_population")
    text <- c(text, paste("    <operator id=\"PopSizeScaler.t:",
      filename_base, "\" parameter=\"@popSize.t:", filename_base,
      "\" scaleFactor=\"0.75\" spec=\"ScaleOperator\" weight=\"3.0\"/>",        # nolint (as this is no absolute path)
      sep = ""))
  }
  text
}







#' Creates the data section of a BEAST2 XML parameter file
#' @param filename_base base of the filenames
#' @param input_fasta_filename name of the FASTA file
#' @export
beast_scriptr_data <- function(
  filename_base,
  input_fasta_filename
) {
  text <- NULL
  text <- c(text, "    <data")
  text <- c(text, paste("id=\"", filename_base, "\"", sep = ""))
  text <- c(text, "name=\"alignment\">")
  sequences_table <- beastscriptr::fasta_file_to_sequences(input_fasta_filename)
  sequences <- cbind(rownames(sequences_table), sequences_table)

  apply(sequences, 1, function(row) {
      nextline <- paste(
        "                    <sequence id=\"seq_",
        row[1],
        "\" taxon=\"",
        row[1],
        "\" totalcount=\"4\" value=\"",
        toupper(row[2]),
        "\"/>",  # nolint (as this is not an absolute path)
        sep = ""
      )
      text <<- c(text, nextline)
    }
  )
  text <- c(text, "                </data>")
  text
}







#' Creates the distribution section of a BEAST2 XML parameter file
#' @param filename_base base of the filenames
#' @param tree_prior the tree prior
#' @export
beast_scriptr_distribution <- function(
  filename_base,
  tree_prior
) {
  text <- NULL
  text <- c(text,
    "    <distribution id=\"posterior\" spec=\"util.CompoundDistribution\">")
  text <- c(text,
    "        <distribution id=\"prior\" spec=\"util.CompoundDistribution\">")

  if (tree_prior == "birth_death") {
    text <- c(text, paste("            <distribution id=\"BirthDeath.t:",
      filename_base, "\" spec=\"beast.evolution.speciation.",
      "BirthDeathGernhard08Model\" birthDiffRate=\"@birthRate2.t:",
      filename_base, "\" relativeDeathRate=\"@relativeDeathRate2.t:",
      filename_base, "\" tree=\"@Tree.t:", filename_base,
      "\"/>", sep = "") # nolint (as this is not an absolute path)
    )
  } else {
    testit::assert(tree_prior == "coalescent_constant_population")
    text <- c(text, paste(
      "            <distribution id=\"CoalescentConstant.t:",
      filename_base, "\" spec=\"Coalescent\">", sep = ""))
    text <- c(text, paste(
      "                <populationModel id=\"ConstantPopulation.t:",
      filename_base, "\" popSize=\"@popSize.t:", filename_base,
      "\" spec=\"ConstantPopulation\"/>",                                       # nolint (as this is not an absolute path)
      sep = ""))
    text <- c(text, paste(
      "                <treeIntervals id=\"TreeIntervals.t:",
      filename_base, "\" spec=\"TreeIntervals\" tree=\"@Tree.t:",
      filename_base, "\"/>", sep = ""))                                         # nolint (as this is no absolute path)
    text <- c(text, "            </distribution>")
  }

  if (tree_prior == "birth_death") {
    text <- c(text, paste("            <prior id=\"BirthRatePrior.t:",
      filename_base, "\" name=\"distribution\" x=\"@birthRate2.t:",
      filename_base, "\">", sep = ""))                                          # nolint (as this is no absolute path)
    text <- c(text,
      paste(
        "                <Uniform id=\"Uniform.0\" ",
        "name=\"distr\" upper=\"1000.0\"/>", sep = ""                           # nolint (as this is no absolute path)
        )
      )
    text <- c(text, "            </prior>")
    text <- c(text, paste("            <prior id=\"DeathRatePrior.t:",
      filename_base, "\" name=\"distribution\" x=\"@relativeDeathRate2.t:",
      filename_base, "\">", sep = ""))
    text <- c(text,
      "                <Uniform id=\"Uniform.01\" name=\"distr\"/>")            # nolint (as this is no absolute path)
  } else {
    testit::assert(tree_prior == "coalescent_constant_population")
    text <- c(text, paste(
      "            <prior id=\"PopSizePrior.t:", filename_base,
      "\" name=\"distribution\" x=\"@popSize.t:",
      filename_base, "\">", sep = ""))
    text <- c(text, "                <OneOnX id=\"OneOnX.0\" name=\"distr\"/>") # nolint (as this is no absolute path)
  }

  text <- c(text, "            </prior>")
  text <- c(text, "        </distribution>")
  text <- c(
      text,
      paste(
        "        <distribution id=\"likelihood\" ",
        "spec=\"util.CompoundDistribution\">", sep = ""                         # nolint (as this is no absolute path)
      )
    )
  text <- c(text, paste("            <distribution id=\"treeLikelihood.",
    filename_base, "\" spec=\"TreeLikelihood\" data=\"@", filename_base,
    "\" tree=\"@Tree.t:", filename_base, "\">", sep = ""))
  text <- c(text, paste("                <siteModel id=\"SiteModel.s:",
    filename_base, "\" spec=\"SiteModel\">", sep = ""))
  text <- c(text, paste("                    <parameter id=\"mutationRate.s:",
    filename_base,
    "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", sep = ""))
  text <- c(text, paste("                    <parameter id=\"gammaShape.s:",
    filename_base,
    "\" estimate=\"false\" name=\"shape\">1.0</parameter>", sep = ""))
  text <- c(text, paste(
    "                    <parameter id=\"proportionInvariant.s:",
    filename_base, "\" estimate=\"false\" lower=\"0.0\" ",
    "name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", sep = ""))
  text <- c(text, paste("                    <substModel id=\"JC69.s:",
    filename_base, "\" spec=\"JukesCantor\"/>", sep = ""))                      # nolint (as this is no absolute path)
  text <- c(text, "                </siteModel>")
  text <- c(text, paste("                <branchRateModel id=\"StrictClock.c:",
    filename_base,
    "\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", sep = ""))
  text <- c(text, paste("                    <parameter id=\"clockRate.c:",
    filename_base,
    "\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", sep = ""))
  text <- c(text, "                </branchRateModel>")
  text <- c(text, "            </distribution>")
  text <- c(text, "        </distribution>")
  text <- c(text, "    </distribution>")
  text
}


#' Creates the map section of a BEAST2 XML parameter file
#' @param filename_base base of the filenames
#' @param initial_phylogeny initial phylogeny
#' @export
beast_scriptr_init <- function(
  filename_base,
  initial_phylogeny
) {
  text <- NULL
  if (!ribir::is_phylogeny(initial_phylogeny)) {
    text <- c(text, paste("    <init id=\"RandomTree.t:", filename_base,
      "\" spec=\"beast.evolution.tree.RandomTree\" estimate=\"false\"",
      " initial=\"@Tree.t:", filename_base, "\" taxa=\"@", filename_base, "\">",
      sep = ""))
    text <- c(text, paste("        <populationModel id=\"ConstantPopulation0.t:",
      filename_base, "\" spec=\"ConstantPopulation\">", sep = ""))
    text <- c(text, paste("            <parameter id=\"randomPopSize.t:",
      filename_base, "\" name=\"popSize\">1.0</parameter>", sep = ""))
    text <- c(text, "        </populationModel>")
    text <- c(text, "    </init>")
  }
  text
}




#' Creates the two logger sections of a BEAST2 XML parameter file
#' @param filename_base filename_base
#' @param tree_prior tree prior
#' @export
beast_scriptr_loggers <- function(
  filename_base,
  tree_prior
) {
  text <- NULL

  text <- c(text, paste("    <logger id=\"tracelog\" fileName=\"",
    filename_base, ".log\" logEvery=\"1000\" model=\"@posterior\" ",
    "sanitiseHeaders=\"true\" sort=\"smart\">", sep = ""))
  text <- c(text, "        <log idref=\"posterior\"/>")                         # nolint (as this is no absolute path)
  text <- c(text, "        <log idref=\"likelihood\"/>")                        # nolint (as this is no absolute path)
  text <- c(text, "        <log idref=\"prior\"/>")                             # nolint (as this is no absolute path)
  text <- c(text, paste("        <log idref=\"treeLikelihood.",
    filename_base, "\"/>", sep = ""))                                           # nolint (as this is no absolute path)
  text <- c(text, paste("        <log id=\"TreeHeight.t:", filename_base,
    "\" spec=\"beast.evolution.tree.TreeHeightLogger\" tree=\"@Tree.t:",
    filename_base, "\"/>", sep = ""))                                           # nolint (as this is no absolute path)

  if (tree_prior == "birth_death") {
    text <- c(text, paste("        <log idref=\"BirthDeath.t:",
      filename_base, "\"/>", sep = ""))                                         # nolint (as this is no absolute path)
    text <- c(text, paste("        <log idref=\"birthRate2.t:",
      filename_base, "\"/>", sep = ""))                                         # nolint (as this is no absolute path)
    text <- c(text, paste("        <log idref=\"relativeDeathRate2.t:",
      filename_base, "\"/>", sep = ""))                                         # nolint (as this is no absolute path)
  } else {
    testit::assert(tree_prior == "coalescent_constant_population")
    text <- c(text, paste("        <parameter idref=\"popSize.t:",
      filename_base, "\" name=\"log\"/>", sep = ""))                            # nolint (as this is no absolute path)
    text <- c(text, paste("        <log idref=\"CoalescentConstant.t:",
      filename_base, "\"/>", sep = ""))                                          # nolint (as this is no absolute path)
  }

  text <- c(text, "    </logger>")
  text <- c(text, "")
  text <- c(text, "    <logger id=\"screenlog\" logEvery=\"1000\">")
  text <- c(text, "        <log idref=\"posterior\"/>")                         # nolint (as this is no absolute path)
  text <- c(text, paste("        <log id=\"ESS.0\" spec=\"util.ESS\" ",
    "arg=\"@posterior\"/>"), sep = "")                                          # nolint (as this is no absolute path)
  text <- c(text, "        <log idref=\"likelihood\"/>")                        # nolint (as this is no absolute path)
  text <- c(text, "        <log idref=\"prior\"/>")                             # nolint (as this is no absolute path)
  text <- c(text, "    </logger>")
  text <- c(text, "")
  text <- c(text, paste("    <logger id=\"treelog.t:", filename_base,
    "\" fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">",
    sep = "")
    )
  text <- c(text, paste("        <log id=\"TreeWithMetaDataLogger.t:",
    filename_base, "\" spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
    "tree=\"@Tree.t:", filename_base, "\"/>",                                   # nolint (as this is no absolute path)
    sep = ""))
  text <- c(text, "    </logger>")
  text
}

#' Creates the map section of a BEAST2 XML parameter file
#' @export
beast_scriptr_map <- function() {
  text <- NULL
  text <- c(text,
    "<map name=\"Uniform\">beast.math.distributions.Uniform</map>")
  text <- c(text,
    "<map name=\"Exponential\">beast.math.distributions.Exponential</map>")
  text <- c(text, paste0("<map name=\"LogNormal\">",
    "beast.math.distributions.LogNormalDistributionModel</map>"))
  text <- c(text, "<map name=\"Normal\">beast.math.distributions.Normal</map>")
  text <- c(text, "<map name=\"Beta\">beast.math.distributions.Beta</map>")
  text <- c(text, "<map name=\"Gamma\">beast.math.distributions.Gamma</map>")
  text <- c(text, paste0("<map name=\"LaplaceDistribution\">",
    "beast.math.distributions.LaplaceDistribution</map>"))
  text <- c(text, "<map name=\"prior\">beast.math.distributions.Prior</map>")
  text <- c(text, paste0("<map name=\"InverseGamma\">",
    "beast.math.distributions.InverseGamma</map>"))
  text <- c(text, "<map name=\"OneOnX\">beast.math.distributions.OneOnX</map>")
  text
}


#' Creates the state section of a BEAST2 XML parameter file
#' @param filename_base filename its base
#' @param tree_prior tree prior
#' @param initial_phylogeny initial phylogeny or NA
#' @export
beast_scriptr_state <- function(
  filename_base,
  tree_prior,
  initial_phylogeny
) {
  text <- NULL
  text <- c(text, "    <state id=\"state\" storeEvery=\"5000\">")

  if (!ribir::is_phylogeny(initial_phylogeny)) {
    # Let BEAST2 use a random tree
    text <- c(text, paste("        <tree id=\"Tree.t:", filename_base,
      "\" name=\"stateNode\">", sep = ""))
    text <- c(text, paste("            <taxonset id=\"TaxonSet.", filename_base,
      "\" spec=\"TaxonSet\">", sep = ""))
    text <- c(text, paste("                <alignment idref=\"", filename_base,
      "\"/>", sep = ""))   # nolint (as this is not an absolute path)
    text <- c(text, "            </taxonset>")
    text <- c(text, "        </tree>")
  }

  if (tree_prior == "birth_death") {
    text <- c(text, paste("        <parameter id=\"birthRate2.t:",
      filename_base,
      "\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>",
      sep = "")
    )
    text <- c(text, paste("        <parameter id=\"relativeDeathRate2.t:",
      filename_base, "\" lower=\"0.0\" name=\"stateNode\"",
      " upper=\"1.0\">0.5</parameter>", sep = ""))
  } else {
    testit::assert(tree_prior == "coalescent_constant_population")
    text <- c(text, paste("        <parameter id=\"popSize.t:",
      filename_base, "\" name=\"stateNode\">0.3</parameter>", sep = ""))
  }

  text <- c(text, "    </state>")
  text
}

#' Creates the xml section of a BEAST2 XML parameter file
#' @export
beast_scriptr_xml <- function() {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>",
    "<beast beautitemplate='Standard' beautistatus='' ",
    "namespace=\"beast.core:beast.evolution.alignment:",
    "beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:",
    "beast.evolution.operators:beast.evolution.sitemodel:",
    "beast.evolution.substitutionmodel:",
    "beast.evolution.likelihood\" version=\"2.0\">"
  )
}
