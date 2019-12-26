#' Create site models from their names
#' @inheritParams default_params_doc
#' @return one or more site models
#' @seealso Use \link{create_site_model} to create a site model
#' @examples
#'   names <- get_site_model_names()
#'   site_models <- create_site_models_from_names(names)
#'
#'   for (i in seq_along(names)) {
#'     testthat::expect_equal(names[i], site_models[[i]]$name)
#'   }
#' @author Richèl J.C. Bilderbeek
#' @export
create_site_models_from_names <- function(site_model_names) {
  site_models <- list()
  for (i in seq_along(site_model_names)) {
    site_model_name <- site_model_names[i]
    site_models[[i]] <- beautier::create_site_model_from_name(site_model_name)
  }
  site_models
}
