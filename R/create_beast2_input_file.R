#' Create a BEAST2 input file
#' @param input_fasta_filenames One or more fasta filename
#' @param site_models one or more site models, as returned by 'create_site_models'
#' @param mcmc_chainlength Length of MCMC chain
#' @param tree_priors On or more tree priors, as returned by 'create_tree_prior'
#' @param output_xml_filename Name of the XML parameter file created by this
#'   function. BEAST2 uses this file as input.
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @param initial_phylogeny the MCMC chain its initial phylogeny. If
#'   this is set to NA, BEAST2 will use a random phylogeny. Else
#'   a phylogeny must be supplied of class ape::phylo.
#' @examples
#'   # Get the filename of an example FASTA file
#'   input_fasta_filename <- get_input_fasta_filename()
#'   testit::assert(file.exists(input_fasta_filename))
#'
#'   # The file created by beastscriptr, a BEAST2 input file
#'   output_xml_filename <- "example_bd.xml"
#'
#'   # Birth-Death tree prior, crown age is estimated
#'   create_beast2_input_file(
#'     input_fasta_filenames = get_input_fasta_filename(),
#'     site_models = create_site_model(name = "JC69"),
#'     mcmc_chainlength = 10000000,
#'     tree_priors = create_tree_prior(name = "birth_death"),
#'     output_xml_filename = output_xml_filename
#'   )
#'   testit::assert(file.exists(output_xml_filename))
#'
#'   # The file created by beastscriptr, a BEAST2 input file
#'   output_xml_filename_fixed <- "example_bd_fixed.xml"
#'
#'   # Birth-Death tree prior, crown age is fixed at 15 time units
#'   create_beast2_input_file(
#'     input_fasta_filenames = get_input_fasta_filename(),
#'     site_models = create_site_model(name = "JC69"),
#'     mcmc_chainlength = 10000000,
#'     tree_priors = create_tree_prior(name = "birth_death"),
#'     output_xml_filename = output_xml_filename_fixed,
#'     fixed_crown_age = TRUE,
#'     initial_phylogeny = beastscriptr::fasta_to_phylo(
#'       input_fasta_filename, crown_age = 15)
#'   )
#'   testit::assert(file.exists(output_xml_filename_fixed))
#' @author Richel Bilderbeek
#' @export
create_beast2_input_file <- function(
  input_fasta_filenames,
  site_models = create_site_model(name = "JC69"),
  mcmc_chainlength,
  tree_priors = create_tree_prior(name = "birth_death"),
  output_xml_filename,
  fixed_crown_age = FALSE,
  initial_phylogeny = NA
) {
  if (!file.exists(input_fasta_filenames)) {
    stop("input_fasta_filenames not found")
  }
  if (!is_valid_site_model(site_models)) {
    stop("invalid site_model")
  }
  if (!is_valid_tree_prior(tree_priors)) {
    stop("invalid tree_prior")
  }
  if (mcmc_chainlength <= 0) {
    stop("mcmc_chainlength must be positive")
  }
  if (!is.logical(fixed_crown_age)) {
    stop("fixed_crown_age must be either TRUE or FALSE")
  }

  # Make a million show as 1000000 instead of 1e+06
  options(scipen = 20)

  text <- NULL
  text <- c(text, beastscriptr::create_beast2_input_file_xml())
  text <- c(text,
    create_beast2_input_file_beast(
      input_fasta_filenames = input_fasta_filenames,
      mcmc_chainlength = mcmc_chainlength,
      tree_priors = tree_priors,
      fixed_crown_age = fixed_crown_age,
      initial_phylogeny = initial_phylogeny
    )
  )

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
create_beast2_input_file_operators <- function(
  filename_base,
  tree_priors,
  fixed_crown_age
) {

  text <- NULL
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator id=\"treeScaler.t:", filename_base,
      "\" spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:",
      filename_base, "\" weight=\"3.0\"/>"))
    text <- c(text, "")
  }
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator id=\"treeRootScaler.t:",
      filename_base,
      "\" spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" ",
      "tree=\"@Tree.t:", filename_base, "\" weight=\"3.0\"/>"))
    text <- c(text, "")
  }
  text <- c(text, paste0("    <operator id=\"UniformOperator.t:", filename_base,
    "\" spec=\"Uniform\" tree=\"@Tree.t:", filename_base,
    "\" weight=\"30.0\"/>"))
  text <- c(text, "")
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator id=\"SubtreeSlide.t:", filename_base,
      "\" spec=\"SubtreeSlide\" tree=\"@Tree.t:", filename_base,
      "\" weight=\"15.0\"/>"))
    text <- c(text, "")
  }

  text <- c(text, paste0("    <operator id=\"narrow.t:", filename_base,
    "\" spec=\"Exchange\" tree=\"@Tree.t:", filename_base,
    "\" weight=\"15.0\"/>"))
  text <- c(text, "")
  text <- c(text, paste0("    <operator id=\"wide.t:", filename_base,
    "\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:", filename_base,
    "\" weight=\"3.0\"/>"))
  text <- c(text, "")
  text <- c(text, paste0("    <operator id=\"WilsonBalding.t:", filename_base,
    "\" spec=\"WilsonBalding\" tree=\"@Tree.t:", filename_base,
    "\" weight=\"3.0\"/>"))
  text <- c(text, "")

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("    <operator id=\"BirthRateScaler.t:",
      filename_base, "\" spec=\"ScaleOperator\" parameter=\"@birthRate2.t:",
      filename_base, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"DeathRateScaler.t:",
      filename_base,
      "\" spec=\"ScaleOperator\" parameter=\"@relativeDeathRate2.t:",
      filename_base, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
  } else {
    testit::assert(tree_priors$name == "coalescent_constant_population")
    text <- c(text, paste0("    <operator id=\"PopSizeScaler.t:",
      filename_base, "\" parameter=\"@popSize.t:", filename_base,
      "\" scaleFactor=\"0.75\" spec=\"ScaleOperator\" weight=\"3.0\"/>",
      sep = ""))
  }
  text
}

#' Creates the beast section of a BEAST2 XML parameter file
#' @param input_fasta_filenames one ore more FASTA filenames
#' @param mcmc_chainlength MCMC chain length
#' @param tree_priors one ore more tree priors,
#'   as returned from 'create_tree_prior'
#' @param fixed_crown_age is the crown age fixed TRUE or FALSE
#' @param initial_phylogeny initial phylogeny or NA
#' @export
create_beast2_input_file_beast <- function(
  input_fasta_filenames,
  mcmc_chainlength,
  tree_priors,
  fixed_crown_age,
  initial_phylogeny
) {
  filename_base <- beastscriptr::remove_file_extension(input_fasta_filenames)
  text <- NULL
  text <- c(text, paste0(
    "<beast beautitemplate='Standard' beautistatus='' ",
    "namespace=\"beast.core:beast.evolution.alignment:",
    "beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:",
    "beast.evolution.operators:beast.evolution.sitemodel:",
    "beast.evolution.substitutionmodel:",
    "beast.evolution.likelihood\" version=\"2.0\">"
  ))

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_file_data(
      filename_base = filename_base,
      input_fasta_filenames = input_fasta_filenames
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

  text <- c(text, beastscriptr::create_beast2_input_file_map())

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_file_run(
      filename_base = filename_base,
      mcmc_chainlength = mcmc_chainlength,
      tree_priors = tree_priors,
      fixed_crown_age = fixed_crown_age,
      initial_phylogeny = initial_phylogeny
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}



#' Creates the data section of a BEAST2 XML parameter file
#' @param filename_base base of the filenames
#' @param input_fasta_filenames name of the FASTA file
#' @export
create_beast2_input_file_data <- function(
  filename_base,
  input_fasta_filenames
) {
  text <- NULL
  text <- c(text, "    <data")
  text <- c(text, paste0("id=\"", filename_base, "\""))
  text <- c(text, "name=\"alignment\">")
  sequences_table <- beastscriptr::fasta_file_to_sequences(input_fasta_filenames)
  sequences <- cbind(rownames(sequences_table), sequences_table)

  apply(sequences, 1, function(row) {
      nextline <- paste0(
        "                    <sequence id=\"seq_",
        row[1],
        "\" taxon=\"",
        row[1],
        "\" totalcount=\"4\" value=\"",
        toupper(row[2]),
        "\"/>",
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
#' @param tree_priors one or more tree priors
#' @export
create_beast2_input_file_distribution <- function(
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


#' Creates the map section of a BEAST2 XML parameter file
#' @param filename_base base of the filenames
#' @param initial_phylogeny initial phylogeny
#' @export
create_beast2_input_file_init <- function(
  filename_base,
  initial_phylogeny
) {
  text <- NULL
  # From https://www.beast2.org/fix-starting-tree/:
  #
  # if there is an initaliser generated by BEAUti of the form
  #
  #   <init estimate="false" id="RandomTree.t:xxx" initial="@Tree.t:xxx"
  #     spec="beast.evolution.tree.RandomTree" taxa="@xxx">
  #   </init>
  #
  # remove this element from the file, otherwise the tree will be Newick tree
  #   will be overwritten by a random tree.
  #
  # In other words: bluntly remove it
  if (!ribir::is_phylogeny(initial_phylogeny)) {
    text <- c(text, paste0("    <init id=\"RandomTree.t:", filename_base,
      "\" spec=\"beast.evolution.tree.RandomTree\" estimate=\"false\"",
      " initial=\"@Tree.t:", filename_base, "\" taxa=\"@", filename_base, "\">"
    ))
    text <- c(text, paste0(
      "        <populationModel id=\"ConstantPopulation0.t:",
      filename_base, "\" spec=\"ConstantPopulation\">"))
    text <- c(text, paste0("            <parameter id=\"randomPopSize.t:",
      filename_base, "\" name=\"popSize\">1.0</parameter>"))
    text <- c(text, "        </populationModel>")
    text <- c(text, "    </init>")
  } else {
    # Do not put initial tree here, but at the state section
  }
  text
}




#' Creates the two logger sections of a BEAST2 XML parameter file
#' @param filename_base filename_base
#' @param tree_priors one or more tree priors
#' @export
create_beast2_input_file_loggers <- function(
  filename_base,
  tree_priors
) {
  text <- NULL

  text <- c(text, paste0("    <logger id=\"tracelog\" fileName=\"",
    filename_base, ".log\" logEvery=\"1000\" model=\"@posterior\" ",
    "sanitiseHeaders=\"true\" sort=\"smart\">"))
  text <- c(text, "        <log idref=\"posterior\"/>")
  text <- c(text, "        <log idref=\"likelihood\"/>")
  text <- c(text, "        <log idref=\"prior\"/>")
  text <- c(text, paste0("        <log idref=\"treeLikelihood.",
    filename_base, "\"/>"))
  text <- c(text, paste0("        <log id=\"TreeHeight.t:", filename_base,
    "\" spec=\"beast.evolution.tree.TreeHeightLogger\" tree=\"@Tree.t:",
    filename_base, "\"/>"))

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("        <log idref=\"BirthDeath.t:",
      filename_base, "\"/>"))
    text <- c(text, paste0("        <log idref=\"birthRate2.t:",
      filename_base, "\"/>"))
    text <- c(text, paste0("        <log idref=\"relativeDeathRate2.t:",
      filename_base, "\"/>"))
  } else {
    testit::assert(tree_priors$name == "coalescent_constant_population")
    text <- c(text, paste0("        <parameter idref=\"popSize.t:",
      filename_base, "\" name=\"log\"/>"))
    text <- c(text, paste0("        <log idref=\"CoalescentConstant.t:",
      filename_base, "\"/>"))
  }

  text <- c(text, "    </logger>")
  text <- c(text, "")
  text <- c(text, "    <logger id=\"screenlog\" logEvery=\"1000\">")
  text <- c(text, "        <log idref=\"posterior\"/>")
  text <- c(text, paste0("        <log id=\"ESS.0\" spec=\"util.ESS\" ",
    "arg=\"@posterior\"/>"))
  text <- c(text, "        <log idref=\"likelihood\"/>")
  text <- c(text, "        <log idref=\"prior\"/>")
  text <- c(text, "    </logger>")
  text <- c(text, "")
  text <- c(text, paste0("    <logger id=\"treelog.t:", filename_base,
    "\" fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">",
    sep = "")
    )
  text <- c(text, paste0("        <log id=\"TreeWithMetaDataLogger.t:",
    filename_base, "\" spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
    "tree=\"@Tree.t:", filename_base, "\"/>",
    sep = ""))
  text <- c(text, "    </logger>")
  text
}

#' Creates the map section of a BEAST2 XML parameter file
#' @export
create_beast2_input_file_map <- function() {
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
#' @param mcmc_chainlength MCMC chain length
#' @param tree_priors one or more tree priors,
#'   as returned by 'create_tree_prior'
#' @param fixed_crown_age is the crown age fixed TRUE or FALSE
#' @param initial_phylogeny initial phylogeny or NA
#' @export
create_beast2_input_file_run <- function(
  filename_base,
  mcmc_chainlength,
  tree_priors,
  fixed_crown_age,
  initial_phylogeny
) {
  text <- NULL

  text <- c(text, paste0("<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"",
    mcmc_chainlength, "\">"))

  text <- c(text,
    create_beast2_input_file_state(
      filename_base = filename_base,
      tree_priors = tree_priors,
      initial_phylogeny = initial_phylogeny
    )
  )

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_file_init(
      filename_base = filename_base,
      initial_phylogeny = initial_phylogeny
    )
  )

  text <- c(text, "")

  text <- c(text,
    create_beast2_input_file_distribution(
      filename_base = filename_base,
      tree_priors = tree_priors
    )
  )

  text <- c(text, "")

  text <- c(text, beastscriptr::create_beast2_input_file_operators(
    filename_base = filename_base,
    tree_priors = tree_priors,
    fixed_crown_age = fixed_crown_age))

  text <- c(text, "")

  text <- c(text, beastscriptr::create_beast2_input_file_loggers(
    filename_base = filename_base,
    tree_priors = tree_priors))

  text <- c(text, "")
  text <- c(text, "</run>")
  text
}

#' Creates the state section of a BEAST2 XML parameter file
#' @param filename_base filename its base
#' @param tree_priors one or more tree priors, as returned
#'   by 'create_tree_prior'
#' @param initial_phylogeny initial phylogeny or NA
#' @export
create_beast2_input_file_state <- function(
  filename_base,
  tree_priors,
  initial_phylogeny
) {
  text <- NULL
  text <- c(text, "    <state id=\"state\" storeEvery=\"5000\">")

  if (!ribir::is_phylogeny(initial_phylogeny)) {
    text <- c(text, paste0("        <tree id=\"Tree.t:",
      filename_base, "\" name=\"stateNode\">"))
    text <- c(text, paste0("            <taxonset id=\"TaxonSet.",
      filename_base, "\" spec=\"TaxonSet\">"))
    text <- c(text, paste0("                <alignment idref=\"",
      filename_base, "\"/>"))
    text <- c(text, "            </taxonset>")
    text <- c(text, "        </tree>")
  } else {
    text <- c(text, paste0("    <stateNode spec=\"beast.util.TreeParser\" ",
        "id=\"Tree.t:", filename_base, "\" IsLabelledNewick=\"true\" ",
        "adjustTipHeights=\"false\" taxa=\"@", filename_base, "\" ",
        "newick=\"", ape::write.tree(initial_phylogeny), "\">"))
    text <- c(text, paste0("    </stateNode>"))
  }

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("        <parameter id=\"birthRate2.t:",
      filename_base,
      "\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>",
      sep = "")
    )
    text <- c(text, paste0("        <parameter id=\"relativeDeathRate2.t:",
      filename_base, "\" lower=\"0.0\" name=\"stateNode\"",
      " upper=\"1.0\">0.5</parameter>"))
  } else {
    testit::assert(tree_priors == "coalescent_constant_population")
    text <- c(text, paste0("        <parameter id=\"popSize.t:",
      filename_base, "\" name=\"stateNode\">0.3</parameter>"))
  }

  text <- c(text, "    </state>")
  text
}

#' Creates the xml section of a BEAST2 XML parameter file
#' @export
create_beast2_input_file_xml <- function() {
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
}
