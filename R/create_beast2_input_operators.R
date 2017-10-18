#' Creates the operators section of a BEAST2 XML parameter file
#' @param fasta_filenames the FASTA filesnames
#' @param tree_priors One or more tree priors, as returned
#'   by 'create_tree_prior'
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @export
create_beast2_input_operators <- function(
  fasta_filenames,
  tree_priors,
  fixed_crown_age
) {
  ids <- beastscriptr::get_file_base_sans_ext(fasta_filenames)

  operator_id_pre <- get_operator_id_pre(tree_priors)

  text <- NULL
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator id=\"", operator_id_pre, "TreeScaler.t:", ids,
      "\" spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:",
      ids, "\" weight=\"3.0\"/>"))
    text <- c(text, "")
  }
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator id=\"", operator_id_pre, "TreeRootScaler.t:",
      ids,
      "\" spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" ",
      "tree=\"@Tree.t:", ids, "\" weight=\"3.0\"/>"))
    text <- c(text, "")
  }
  text <- c(text, paste0("    <operator id=\"", operator_id_pre, "UniformOperator.t:", ids,
    "\" spec=\"Uniform\" tree=\"@Tree.t:", ids,
    "\" weight=\"30.0\"/>"))
  text <- c(text, "")
  if (fixed_crown_age == FALSE) {
    text <- c(text, paste0("    <operator id=\"", operator_id_pre, "SubtreeSlide.t:", ids,
      "\" spec=\"SubtreeSlide\" tree=\"@Tree.t:", ids,
      "\" weight=\"15.0\"/>"))
    text <- c(text, "")
  }

  text <- c(text, paste0("    <operator id=\"", operator_id_pre, "Narrow.t:", ids,
    "\" spec=\"Exchange\" tree=\"@Tree.t:", ids,
    "\" weight=\"15.0\"/>"))
  text <- c(text, "")
  text <- c(text, paste0("    <operator id=\"", operator_id_pre, "Wide.t:", ids,
    "\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:", ids,
    "\" weight=\"3.0\"/>"))
  text <- c(text, "")
  text <- c(text, paste0("    <operator id=\"", operator_id_pre, "WilsonBalding.t:", ids,
    "\" spec=\"WilsonBalding\" tree=\"@Tree.t:", ids,
    "\" weight=\"3.0\"/>"))
  text <- c(text, "")

  if (tree_priors$name == "birth_death") {
    text <- c(text, paste0("    <operator id=\"BirthRateScaler.t:",
      ids, "\" spec=\"ScaleOperator\" parameter=\"@BDBirthRate.t:",
      ids, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
    text <- c(text, "")
    text <- c(text, paste0("    <operator id=\"DeathRateScaler.t:",
      ids,
      "\" spec=\"ScaleOperator\" parameter=\"@BDDeathRate.t:",
      ids, "\" scaleFactor=\"0.75\" weight=\"3.0\"/>"))
  } else {
    testit::assert(tree_priors$name == "coalescent_constant_population")
    text <- c(text, paste0("    <operator id=\"PopSizeScaler.t:",
      ids, "\" parameter=\"@popSize.t:", ids,
      "\" scaleFactor=\"0.75\" spec=\"ScaleOperator\" weight=\"3.0\"/>",
      sep = ""))
  }
  text
}
