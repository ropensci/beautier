#' Create a BEAST2 XML parameter file
#' @param input_fasta_filename Filename of a fasta file
#' @param mcmc_chainlength Length of MCMC chain
#' @param tree_prior The tree prior, can be 'birth_death' or 'coalescent_constant_population'
#' @param date_str The date of today in some unknown format
#' @param  output_xml_filename Filename of the XML parameter file created
#' @param verbose give verbose output, should be TRUE or FALSE
#' @export
beast_scriptr <- function(
  input_fasta_filename,
  mcmc_chainlength,
  tree_prior,
  date_str,
  output_xml_filename
) {
  if (!file.exists(input_fasta_filename)) {
    stop("beast_scriptr: ",
         "input_fasta_filename with path '",
         input_fasta_filename,
         "' is not found"
    )
  }
  if (tree_prior != "birth_death" &&
      tree_prior != "coalescent_constant_population") {
    stop("beast_scriptr: ",
         "tree_prior must be 'birth_death' ",
         "or 'coalescent_constant_population', ",
         "instead of '",
         tree_prior,
         "'"
    )
  }
  if (mcmc_chainlength <= 0) {
    stop("beast_scriptr: ",
         "mcmc_chainlength must be positive, ",
         "instead of '",
         mcmc_chainlength,
         "'"
    )
  }
  if (verbose != TRUE && verbose != FALSE) {
    stop(
      "convert_alignment_to_beast_input_file: ",
      "verbose should be TRUE or FALSE"
    )
  }

  # Make a million show as 1000000 instead of 1e+06
  options(scipen = 20)

  text <- NULL
  text <- c(text,
    paste("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>",
      "<beast beautitemplate='Standard' beautistatus='' ",
      "namespace=\"beast.core:beast.evolution.alignment:",
      "beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:",
      "beast.evolution.operators:beast.evolution.sitemodel:",
      "beast.evolution.substitutionmodel:",
      "beast.evolution.likelihood\" version=\"2.0\">", sep = "")
    )
  text <- c(text, "")
  text <- c(text, "")
  filename_base <- remove_file_extension(input_fasta_filename)

  text <- c(text, "    <data")
  text <- c(text, paste("id=\"", filename_base, "\"", sep = ""))
  text <- c(text, "name=\"alignment\">")
  sequences_table <- convert_fasta_file_to_sequences(input_fasta_filename)
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
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
  text <- c(text,
    "<map name=\"Uniform\">beast.math.distributions.Uniform</map>")
  text <- c(text,
    "<map name=\"Exponential\">beast.math.distributions.Exponential</map>")
  text <- c(text, paste("<map name=\"LogNormal\">",
    "beast.math.distributions.LogNormalDistributionModel</map>", sep = ""))
  text <- c(text, "<map name=\"Normal\">beast.math.distributions.Normal</map>")
  text <- c(text, "<map name=\"Beta\">beast.math.distributions.Beta</map>")
  text <- c(text, "<map name=\"Gamma\">beast.math.distributions.Gamma</map>")
  text <- c(text, paste("<map name=\"LaplaceDistribution\">",
    "beast.math.distributions.LaplaceDistribution</map>", sep = ""))
  text <- c(text, "<map name=\"prior\">beast.math.distributions.Prior</map>")
  text <- c(text, paste("<map name=\"InverseGamma\">",
    "beast.math.distributions.InverseGamma</map>", sep = ""))
  text <- c(text, "<map name=\"OneOnX\">beast.math.distributions.OneOnX</map>")
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, paste("<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"",
    mcmc_chainlength, "\">", sep = ""))
  text <- c(text, "    <state id=\"state\" storeEvery=\"5000\">")
  text <- c(text, paste("        <tree id=\"Tree.t:", filename_base,
    "\" name=\"stateNode\">", sep = ""))
  text <- c(text, paste("            <taxonset id=\"TaxonSet.", filename_base,
    "\" spec=\"TaxonSet\">", sep = ""))
  text <- c(text, paste("                <alignment idref=\"", filename_base,
    "\"/>", sep = ""))   # nolint (as this is not an absolute path)
  text <- c(text, "            </taxonset>")
  text <- c(text, "        </tree>")

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
  text <- c(text, "")
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
  text <- c(text, "")
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
  text <- c(text, "")
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

  text <- c(text, "")
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
  text <- c(text, "")
  text <- c(text, "</run>")
  text <- c(text, "")
  text <- c(text, "</beast>")

  # Write to file
  my_file <- file(output_xml_filename)
  writeLines(text, my_file)
  close(my_file)
}


#' Convert a FASTA file to a table of sequences
#' @param fasta_filename Name of an existing FASTA file
#' @return a table of sequences
#' @export
convert_fasta_file_to_sequences <- function(fasta_filename) {
  if (!file.exists(fasta_filename)) {
    stop("convert_fasta_file_to_sequences: ",
         "fasta_filename with path '",
         fasta_filename,
         "' is not found"
    )
  }

  # Read the file
  sequences_dnabin <- ape::read.FASTA(fasta_filename)
  testit::assert(class(sequences_dnabin) == "DNAbin")

  # Convert the file to a table with labels and sequences
  labels <- names(sequences_dnabin)
  chars <- as.character(sequences_dnabin)
  sequences <- NULL

  for (line in chars) {
    sequence <- utils::capture.output(cat(line, sep = ""))
    sequences <- c(sequences, sequence)
  }

  table <- data.frame(sequences, row.names = labels)
  return(table)
}

#' Remove a file extension
#' @param filename A filename
#' @return That filename without its extension
#' @export
remove_file_extension <- function(filename) {
  return(strsplit(filename, "\\.")[[1]][1])
}
