#' Initializes all site models
#' @param site_models a list of one or more site models to be initialized.
#'   Clock priors can be created using \code{\link{create_site_model}}
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_ids}})
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized site models
#' @author Richel J.C. Bilderbeek
init_site_models <- function(
  site_models,
  ids,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(length(site_models) == length(ids))

  for (i in seq_along(site_models)) {
    site_model <- site_models[[i]]
    testit::assert(beautier::is_site_model(site_model))

    if (beautier::is_gtr_site_model(site_model)) {
      # Nothing to initialize (for now)
    } else if (beautier::is_hky_site_model(site_model)) {

      # HKY
      if (!is_init_hky_site_model(site_model)) {

        site_model <- init_hky_site_model( # nolint internal function call
          site_model,
          distr_id = distr_id,
          param_id = param_id
        )
        distr_id <- distr_id  # Has one distributions
        param_id <- param_id + beautier::get_distr_n_params(
          site_model$kappa_prior)
      }

    } else if (beautier::is_jc69_site_model(site_model)) {
      # Nothing to initialize (for now)
    } else {
      testit::assert(beautier::is_tn93_site_model(site_model))
      # Nothing to initialize (for now)
    }
    site_models[[i]] <- site_model
  }
  site_models
}

#' Initializes a Relaxed Log-Normal site model
#' @param hky_site_model a Relaxed Log-Normal site model,
#'   as returned by \code{\link{create_hky_site_model}}
#' @inheritParams init_site_models
#' @return an initialized Relaxed Log-Normal site model
#' @author Richel J.C. Bilderbeek
init_hky_site_model <- function(
  hky_site_model,
  distr_id,
  param_id
) {
  testit::assert(beautier::is_hky_site_model(hky_site_model))

  result <- create_hky_site_model(
    kappa = hky_site_model$kappa,
    gamma_site_model = hky_site_model$gamma_site_model,
    kappa_prior = init_distr(
      hky_site_model$kappa_prior,
      distr_id,
      param_id
    )
  )

  result
}
