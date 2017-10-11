#' Creates the operators section of a BEAST2 XML parameter file
#' @param filename_base base of the filenames
#' @param tree_priors One or more tree priors, as returned
#'   by 'create_tree_prior'
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @export
create_beast2_input_operators <- function(
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
