#' Are there any shared RLN clock models?
#' @inheritParams default_params_doc
#' @note this must be FALSE, as BEAUti rejects this as well
#' @author Richel J.C. Bilderbeek
has_shared_rln_clock_models <- function(
  clock_models
) {
  rln_clock_models <- clock_models[are_rln_clock_models(clock_models)] # nolint internal function
  rln_ids <- get_clock_models_ids(rln_clock_models) # nolint internal function
  length(unique(rln_ids)) != length(rln_ids)
}
