#' Get the first site model of each ID
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
get_unlinked_site_models <- function(site_models) {
  testit::assert(are_site_models(site_models))
  results <- list()
  ids <- NULL
  for (site_model in site_models) {
    id <- site_model$id
    if (!id %in% ids) {
      ids <- c(ids, id)
      results[[length(ids)]] <- site_model
    }
  }

  testit::assert(are_site_models(results))
  results
}
