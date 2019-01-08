#' Create site models from their names
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_site_models_from_names <- function(site_model_names) {
  site_models <- list()
  for (i in seq_along(site_model_names)) {
    site_model_name <- site_model_names[i]
    site_models[[i]] <- create_site_model_from_name(site_model_name)
  }
  site_models
}
