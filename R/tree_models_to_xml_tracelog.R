#' @note use site_models just because it contains all IDs
#' @inheritParams default_params_doc
tree_models_to_xml_tracelog <- function(
  site_models
) {
  text <- NULL
  for (site_model in site_models) {
    id <- site_model$id
    text <- c(text, paste0("<log idref=\"treeLikelihood.", id, "\"/>"))
    text <- c(text, paste0("<log id=\"TreeHeight.t:", id, "\" ",
      "spec=\"beast.evolution.tree.TreeHeightLogger\" ",
      "tree=\"@Tree.t:", id, "\"/>")
    )
  }
  text
}
