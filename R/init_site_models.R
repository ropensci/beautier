#' Initializes all site models
#' @inheritParams default_params_doc
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
    testit::assert(is_site_model(site_model))

    if (is_gtr_site_model(site_model)) {

      # GTR
      if (!is_init_gtr_site_model(site_model)) {
        site_model <- init_gtr_site_model( # nolint internal function call
          site_model,
          distr_id = distr_id,
          param_id = param_id
        )
      }

    } else if (is_hky_site_model(site_model)) {

      # HKY
      if (!is_init_hky_site_model(site_model)) {

        site_model <- init_hky_site_model( # nolint internal function call
          site_model,
          distr_id = distr_id,
          param_id = param_id
        )
      }

    } else if (is_jc69_site_model(site_model)) {
      # Nothing to initialize (for now)
    } else {
      testit::assert(is_tn93_site_model(site_model))
      site_model <- init_tn93_site_model( # nolint internal function call
        site_model,
        distr_id = distr_id,
        param_id = param_id
      )
    }

    distr_id <- distr_id + beautier::get_site_model_n_distrs(site_model)
    param_id <- param_id + beautier::get_site_model_n_params(site_model)

    if (is.na(site_model$id)) site_model$id <- ids[i]
    site_models[[i]] <- site_model
  }
  site_models
}

#' Initializes a GTR site model
#' @inheritParams default_params_doc
#' @return an initialized GTR site model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   gtr_site_model <- create_gtr_site_model()
#'   testit::assert(!beautier:::is_init_gtr_site_model(gtr_site_model))
#'   gtr_site_model <- beautier:::init_gtr_site_model(gtr_site_model)
#'   testit::assert(beautier:::is_init_gtr_site_model(gtr_site_model))
init_gtr_site_model <- function(
  gtr_site_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(is_gtr_site_model(gtr_site_model))

  rate_ac_prior_distr <- gtr_site_model$rate_ac_prior_distr
  rate_ag_prior_distr <- gtr_site_model$rate_ag_prior_distr
  rate_at_prior_distr <- gtr_site_model$rate_at_prior_distr
  rate_cg_prior_distr <- gtr_site_model$rate_cg_prior_distr
  rate_gt_prior_distr <- gtr_site_model$rate_gt_prior_distr
  rate_ac_param <- gtr_site_model$rate_ac_param
  rate_ag_param <- gtr_site_model$rate_ag_param
  rate_at_param <- gtr_site_model$rate_at_param
  rate_cg_param <- gtr_site_model$rate_cg_param
  rate_ct_param <- gtr_site_model$rate_ct_param
  rate_gt_param <- gtr_site_model$rate_gt_param

  if (!is_init_distr(rate_ac_prior_distr)) {
    rate_ac_prior_distr <- init_distr(
      rate_ac_prior_distr, distr_id = distr_id, param_id = param_id)
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(rate_ac_prior_distr)
  }
  if (!is_init_distr(rate_ag_prior_distr)) {
    rate_ag_prior_distr <- init_distr(
      rate_ag_prior_distr, distr_id = distr_id, param_id = param_id)
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(rate_ag_prior_distr)
  }
  if (!is_init_distr(rate_at_prior_distr)) {
    rate_at_prior_distr <- init_distr(
      rate_at_prior_distr, distr_id = distr_id, param_id = param_id)
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(rate_at_prior_distr)
  }
  if (!is_init_distr(rate_cg_prior_distr)) {
    rate_cg_prior_distr <- init_distr(
      rate_cg_prior_distr, distr_id = distr_id, param_id = param_id)
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(rate_cg_prior_distr)
  }
  if (!is_init_distr(rate_gt_prior_distr)) {
    rate_gt_prior_distr <- init_distr(
      rate_gt_prior_distr, distr_id = distr_id, param_id = param_id)
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(rate_gt_prior_distr)
  }
  if (!is_init_param(rate_ac_param)) {
    rate_ac_param <- init_param(rate_ac_param, id = param_id) # nolint internal function
    param_id <- param_id + 1
  }
  if (!is_init_param(rate_ag_param)) {
    rate_ag_param <- init_param(rate_ag_param, id = param_id) # nolint internal function
    param_id <- param_id + 1
  }
  if (!is_init_param(rate_at_param)) {
    rate_at_param <- init_param(rate_at_param, id = param_id) # nolint internal function
    param_id <- param_id + 1
  }
  if (!is_init_param(rate_cg_param)) {
    rate_cg_param <- init_param(rate_cg_param, id = param_id) # nolint internal function
    param_id <- param_id + 1
  }
  if (!is_init_param(rate_ct_param)) {
    rate_ct_param <- init_param(rate_ct_param, id = param_id) # nolint internal function
    param_id <- param_id + 1
  }
  if (!is_init_param(rate_gt_param)) {
    rate_gt_param <- init_param(rate_gt_param, id = param_id) # nolint internal function
    param_id <- param_id + 1
  }

  result <- create_gtr_site_model(
    id = gtr_site_model$id,
    gamma_site_model = gtr_site_model$gamma_site_model,
    rate_ac_prior_distr = rate_ac_prior_distr,
    rate_ag_prior_distr = rate_ag_prior_distr,
    rate_at_prior_distr = rate_at_prior_distr,
    rate_cg_prior_distr = rate_cg_prior_distr,
    rate_gt_prior_distr = rate_gt_prior_distr,
    rate_ac_param = rate_ac_param,
    rate_ag_param = rate_ag_param,
    rate_at_param = rate_at_param,
    rate_cg_param = rate_cg_param,
    rate_ct_param = rate_ct_param,
    rate_gt_param = rate_gt_param,
    freq_equilibrium = gtr_site_model$freq_equilibrium
  )

  testit::assert(is_gtr_site_model(result))
  result
}

#' Initializes an HKY site model
#' @inheritParams default_params_doc
#' @return an initialized HKY site model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   hky_site_model <- create_hky_site_model()
#'   testit::assert(!beautier:::is_init_hky_site_model(hky_site_model))
#'   hky_site_model <- beautier:::init_hky_site_model(hky_site_model)
#'   testit::assert(beautier:::is_init_hky_site_model(hky_site_model))
init_hky_site_model <- function(
  hky_site_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(is_hky_site_model(hky_site_model))

  result <- create_hky_site_model(
    id = hky_site_model$id,
    kappa = hky_site_model$kappa,
    gamma_site_model = hky_site_model$gamma_site_model,
    kappa_prior_distr = init_distr(
      hky_site_model$kappa_prior,
      distr_id,
      param_id
    ),
    freq_equilibrium = hky_site_model$freq_equilibrium
  )

  result
}

#' Initializes a TN93 site model
#' @inheritParams default_params_doc
#' @return an initialized TN93 site model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   tn93_site_model <- create_tn93_site_model()
#'   testit::assert(!beautier:::is_init_tn93_site_model(tn93_site_model))
#'   tn93_site_model <- beautier:::init_tn93_site_model(tn93_site_model)
#'   testit::assert(beautier:::is_init_tn93_site_model(tn93_site_model))
init_tn93_site_model <- function(
  tn93_site_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(is_tn93_site_model(tn93_site_model))
  kappa_1_prior_distr <- init_distr(
    tn93_site_model$kappa_1_prior_distr,
    distr_id = distr_id,
    param_id = param_id
  )
  distr_id <- distr_id + 1
  param_id <- param_id + beautier::get_distr_n_params(
    tn93_site_model$kappa_1_prior_distr)
  kappa_2_prior_distr <- init_distr(
    tn93_site_model$kappa_2_prior_distr,
    distr_id = distr_id,
    param_id = param_id
  )
  param_id <- param_id + beautier::get_distr_n_params(
    tn93_site_model$kappa_2_prior_distr)
  kappa_1_param <- init_param(tn93_site_model$kappa_1_param, id = param_id) # nolint internal function
  param_id <- param_id + 1
  kappa_2_param <- init_param(tn93_site_model$kappa_2_param, id = param_id) # nolint internal function

  create_tn93_site_model(
    id = tn93_site_model$id,
    gamma_site_model = tn93_site_model$gamma_site_model,
    kappa_1_prior_distr = kappa_1_prior_distr,
    kappa_2_prior_distr = kappa_2_prior_distr,
    kappa_1_param = kappa_1_param,
    kappa_2_param = kappa_2_param,
    freq_equilibrium = tn93_site_model$freq_equilibrium
  )
}
