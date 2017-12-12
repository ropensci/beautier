tree_priors_to_xml_tracelog <- function(
  tree_priors
) {
  text <- NULL
  for (tree_prior in tree_priors) {
    text <- c(text,
      tree_prior_to_xml_tracelog(tree_prior)
    )
  }
  text
}
