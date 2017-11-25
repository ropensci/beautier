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

      # GTR
      if (!is_init_gtr_site_model(site_model)) {
        site_model <- init_gtr_site_model( # nolint internal function call
          site_model,
          distr_id = distr_id,
          param_id = param_id
        )
      }

    } else if (beautier::is_hky_site_model(site_model)) {

      # HKY
      if (!is_init_hky_site_model(site_model)) {

        site_model <- init_hky_site_model( # nolint internal function call
          site_model,
          distr_id = distr_id,
          param_id = param_id
        )
      }

    } else if (beautier::is_jc69_site_model(site_model)) {
      # Nothing to initialize (for now)
    } else {
      testit::assert(beautier::is_tn93_site_model(site_model))
      site_model <- init_tn93_site_model( # nolint internal function call
        site_model,
        distr_id = distr_id,
        param_id = param_id
      )
    }

    distr_id <- distr_id + beautier::get_site_model_n_distrs(site_model)
    param_id <- param_id + beautier::get_site_model_n_params(site_model)

    site_models[[i]] <- site_model
  }
  site_models
}

#' Initializes a GTR site model
#' @param gtr_site_model a GTR site model,
#'   as returned by \code{\link{create_gtr_site_model}}
#' @inheritParams init_site_models
#' @return an initialized GTR site model
#' @author Richel J.C. Bilderbeek
init_gtr_site_model <- function(
  gtr_site_model,
  distr_id,
  param_id
) {
  testit::assert(beautier::is_gtr_site_model(gtr_site_model))

  result <- create_gtr_site_model(
    gamma_site_model = gtr_site_model$gamma_site_model,
    gamma_0_alpha = init_param(gtr_site_model$gamma_0_alpha, id = param_id + 1),
    gamma_0_beta = init_param(gtr_site_model$gamma_0_beta, id = param_id + 2),
    gamma_1_alpha = init_param(gtr_site_model$gamma_1_alpha, id = param_id + 3),
    gamma_1_beta = init_param(gtr_site_model$gamma_1_beta, id = param_id + 4),
    gamma_2_alpha = init_param(gtr_site_model$gamma_2_alpha, id = param_id + 5),
    gamma_2_beta = init_param(gtr_site_model$gamma_2_beta, id = param_id + 6),
    gamma_3_alpha = init_param(gtr_site_model$gamma_3_alpha, id = param_id + 7),
    gamma_3_beta = init_param(gtr_site_model$gamma_3_beta, id = param_id + 8),
    gamma_5_alpha = init_param(gtr_site_model$gamma_5_alpha, id = param_id + 9),
    gamma_5_beta = init_param(gtr_site_model$gamma_5_beta, id = param_id + 10)
  )

  testit::assert(beautier::is_gtr_site_model(result))
  result
}

#' Initializes an HKY site model
#' @param hky_site_model an HKY site model,
#'   as returned by \code{\link{create_hky_site_model}}
#' @inheritParams init_site_models
#' @return an initialized HKY site model
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
    kappa_prior_distr = init_distr(
      hky_site_model$kappa_prior,
      distr_id,
      param_id
    )
  )

  result
}

#' Initializes a TN93 site model
#' @param tn93_site_model a TN93 site model,
#'   as returned by \code{\link{create_tn93_site_model}}
#' @inheritParams init_site_models
#' @return an initialized TN93 site model
#' @author Richel J.C. Bilderbeek
init_tn93_site_model <- function(
  tn93_site_model,
  distr_id,
  param_id
) {
  testit::assert(beautier::is_tn93_site_model(tn93_site_model))

  create_tn93_site_model(
    gamma_site_model = tn93_site_model$gamma_site_model,
    kappa_1_prior_distr = init_distr(
      tn93_site_model$kappa_1_prior_distr,
      distr_id,
      param_id
    ),
    kappa_2_prior_distr = init_distr(
      tn93_site_model$kappa_2_prior_distr,
      distr_id + 1,
      param_id + get_distr_n_params(tn93_site_model$kappa_1_prior_distr)
    )
  )
}
