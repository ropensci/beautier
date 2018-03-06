#' Creates n Yule tree priors
#' @param ids the alignment IDs
#' @return a list of Yule tree_prior objects
#' @examples
#'   fasta_filenames <- get_beautier_paths(
#'     c("anthus_aco.fas", "anthus_nd2.fas")
#'   )
#'
#'   tree_priors <- create_yule_tree_priors(ids = get_ids(fasta_filenames))
#'
#'   create_beast2_input_file(
#'     fasta_filenames,
#'     "create_yule_tree_priors.xml",
#'     tree_priors = tree_priors
#'   )
#'   testit::assert(file.exists("create_yule_tree_priors.xml"))
#' @export
create_yule_tree_priors <- function(ids) {
  n <- length(ids)
  ms <- list()
  for (i in seq(1, n)) {
    ms[[i]] <- beautier::create_yule_tree_prior(id = ids[i])
  }
  ms
}
