#' Initializes all site models
#' @inheritParams default_params_doc
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized site models
#' @author Richèl J.C. Bilderbeek
#' @noRd
init_site_models <- function(
  site_models,
  ids,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(are_site_models(site_models)) # nolint beautier function
  testit::assert(length(site_models) == length(ids))

  for (i in seq_along(site_models)) {
    site_model <- site_models[[i]]
    testit::assert(is_site_model(site_model)) # nolint beautier function
    if (is_gtr_site_model(site_model)) { # nolint beautier function
      # GTR
      site_model <- init_gtr_site_model( # nolint beautier function call
        site_model,
        distr_id = distr_id,
        param_id = param_id
      )
    } else if (is_hky_site_model(site_model)) { # nolint beautier function
      # HKY
      site_model <- init_hky_site_model( # nolint beautier function call
        site_model,
        distr_id = distr_id,
        param_id = param_id
      )
    } else if (is_jc69_site_model(site_model)) { # nolint beautier function
      # JC69
      site_model <- init_jc69_site_model( # nolint beautier function call
        site_model,
        distr_id = distr_id,
        param_id = param_id
      )
    } else {
      testit::assert(is_tn93_site_model(site_model)) # nolint beautier function
      site_model <- init_tn93_site_model( # nolint beautier function call
        site_model,
        distr_id = distr_id,
        param_id = param_id
      )
    }

    distr_id <- distr_id + get_site_model_n_distrs(site_model) # nolint beautier function
    param_id <- param_id + get_site_model_n_params(site_model) # nolint beautier function

    if (is_one_na(site_model$id)) site_model$id <- ids[i] # nolint beautier function call
    testit::assert(is_init_site_model(site_model)) # nolint beautier function call
    site_models[[i]] <- site_model
  }
  site_models
}

#' Initializes a GTR site model
#' @inheritParams default_params_doc
#' @return an initialized GTR site model
#' @author Richèl J.C. Bilderbeek
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
  testit::assert(is_gtr_site_model(gtr_site_model)) # nolint beautier function

  # Initialize gamma site model, if any
  if (!is_one_na(gtr_site_model$gamma_site_model$gamma_shape_prior_distr)) { # nolint beautier function
    if (!is_init_distr(gtr_site_model$gamma_site_model$gamma_shape_prior_distr)) { # nolint beautier function
      gtr_site_model$gamma_site_model$gamma_shape_prior_distr <- init_distr( # nolint beautier function
        gtr_site_model$gamma_site_model$gamma_shape_prior_distr,
        distr_id = distr_id,
        param_id = param_id
      )
      distr_id <- distr_id + 1
      param_id <- param_id + get_distr_n_params( # nolint beautier function
        gtr_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    }
  }

  if (!is_init_distr(gtr_site_model$rate_ac_prior_distr)) { # nolint beautier function
    gtr_site_model$rate_ac_prior_distr <- init_distr( # nolint beautier function
      gtr_site_model$rate_ac_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + get_distr_n_params( # nolint beautier function
      gtr_site_model$rate_ac_prior_distr
    )
  }
  if (!is_init_distr(gtr_site_model$rate_ag_prior_distr)) { # nolint beautier function
    gtr_site_model$rate_ag_prior_distr <- init_distr( # nolint beautier function
      gtr_site_model$rate_ag_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + get_distr_n_params( # nolint beautier function
      gtr_site_model$rate_ag_prior_distr
    )
  }
  if (!is_init_distr(gtr_site_model$rate_at_prior_distr)) { # nolint beautier function
    gtr_site_model$rate_at_prior_distr <- init_distr( # nolint beautier function
      gtr_site_model$rate_at_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + get_distr_n_params( # nolint beautier function
      gtr_site_model$rate_at_prior_distr
    )
  }
  if (!is_init_distr(gtr_site_model$rate_cg_prior_distr)) { # nolint beautier function
    gtr_site_model$rate_cg_prior_distr <- init_distr( # nolint beautier function
      gtr_site_model$rate_cg_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + get_distr_n_params( # nolint beautier function
      gtr_site_model$rate_cg_prior_distr
    )
  }
  if (!is_init_distr(gtr_site_model$rate_gt_prior_distr)) { # nolint beautier function
    gtr_site_model$rate_gt_prior_distr <- init_distr( # nolint beautier function
      gtr_site_model$rate_gt_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + get_distr_n_params( # nolint beautier function
      gtr_site_model$rate_gt_prior_distr
    )
  }
  if (!is_init_param(gtr_site_model$rate_ac_param)) { # nolint beautier function
    gtr_site_model$rate_ac_param <- init_param( # nolint beautier function
      gtr_site_model$rate_ac_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  if (!is_init_param(gtr_site_model$rate_ag_param)) { # nolint beautier function
    gtr_site_model$rate_ag_param <- init_param( # nolint beautier function
      gtr_site_model$rate_ag_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  if (!is_init_param(gtr_site_model$rate_at_param)) { # nolint beautier function
    gtr_site_model$rate_at_param <- init_param( # nolint beautier function
      gtr_site_model$rate_at_param, id = param_id
    )
    param_id <- param_id + 1
  }
  if (!is_init_param(gtr_site_model$rate_cg_param)) { # nolint beautier function
    gtr_site_model$rate_cg_param <- init_param( # nolint beautier function
      gtr_site_model$rate_cg_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  if (!is_init_param(gtr_site_model$rate_ct_param)) { # nolint beautier function
    gtr_site_model$rate_ct_param <- init_param( # nolint beautier function
      gtr_site_model$rate_ct_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  if (!is_init_param(gtr_site_model$rate_gt_param)) { # nolint beautier function
    gtr_site_model$rate_gt_param <- init_param( # nolint beautier function
      gtr_site_model$rate_gt_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  testit::assert(is_gtr_site_model(gtr_site_model)) # nolint beautier function
  testit::assert(is_init_gamma_site_model(gtr_site_model$gamma_site_model)) # nolint beautier function
  testit::assert(is_init_gtr_site_model(gtr_site_model)) # nolint beautier function
  gtr_site_model
}

#' Initializes an HKY site model
#' @inheritParams default_params_doc
#' @return an initialized HKY site model
#' @author Richèl J.C. Bilderbeek
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
  testit::assert(is_hky_site_model(hky_site_model)) # nolint beautier function

  # Initialize gamma site model, if any
  if (!is_one_na(hky_site_model$gamma_site_model$gamma_shape_prior_distr)) { # nolint beautier function
    if (!is_init_distr( # nolint beautier function
      hky_site_model$gamma_site_model$gamma_shape_prior_distr
    )) {
      hky_site_model$gamma_site_model$gamma_shape_prior_distr <- init_distr( # nolint beautier function
        hky_site_model$gamma_site_model$gamma_shape_prior_distr,
        distr_id = distr_id,
        param_id = param_id
      )
      distr_id <- distr_id + 1
      param_id <- param_id + get_distr_n_params( # nolint beautier function
        hky_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    }
  }

  # kappa_prior_distr
  if (!is_init_distr(hky_site_model$kappa_prior_distr)) { # nolint beautier function
    hky_site_model$kappa_prior_distr <- init_distr( # nolint beautier function
      hky_site_model$kappa_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + get_distr_n_params( # nolint beautier function
      hky_site_model$kappa_prior_distr
    )
  }

  testit::assert(is_init_gamma_site_model(hky_site_model$gamma_site_model)) # nolint beautier function
  testit::assert(is_init_hky_site_model(hky_site_model)) # nolint beautier function
  hky_site_model
}


#' Initializes a JC69 site model
#' @inheritParams default_params_doc
#' @return an initialized HKY site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   hky_site_model <- create_hky_site_model()
#'   testit::assert(!beautier:::is_init_hky_site_model(hky_site_model))
#'   hky_site_model <- beautier:::init_hky_site_model(hky_site_model)
#'   testit::assert(beautier:::is_init_hky_site_model(hky_site_model))
init_jc69_site_model <- function(
  jc69_site_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(is_jc69_site_model(jc69_site_model)) # nolint beautier function

  # Initialize gamma site model, if any
  if (!is_one_na(jc69_site_model$gamma_site_model$gamma_shape_prior_distr)) { # nolint beautier function
    if (
      !is_init_distr(jc69_site_model$gamma_site_model$gamma_shape_prior_distr) # nolint beautier function
    ) {
      jc69_site_model$gamma_site_model$gamma_shape_prior_distr <- init_distr( # nolint beautier function
        jc69_site_model$gamma_site_model$gamma_shape_prior_distr,
        distr_id = distr_id,
        param_id = param_id
      )
      distr_id <- distr_id + 1
      param_id <- param_id + get_distr_n_params( # nolint beautier function
        jc69_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    }
  }

  testit::assert(is_init_gamma_site_model(jc69_site_model$gamma_site_model)) # nolint beautier function
  testit::assert(is_init_jc69_site_model(jc69_site_model)) # nolint beautier function
  jc69_site_model
}



#' Initializes a TN93 site model
#' @inheritParams default_params_doc
#' @return an initialized TN93 site model
#' @author Richèl J.C. Bilderbeek
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
  testit::assert(is_tn93_site_model(tn93_site_model)) # nolint beautier function

  # Initialize gamma site model, if any
  if (!is_one_na(tn93_site_model$gamma_site_model$gamma_shape_prior_distr)) { # nolint beautier function
    if (
      !is_init_distr(tn93_site_model$gamma_site_model$gamma_shape_prior_distr) # nolint beautier function
    ) {
      tn93_site_model$gamma_site_model$gamma_shape_prior_distr <- init_distr( # nolint beautier function
        tn93_site_model$gamma_site_model$gamma_shape_prior_distr,
        distr_id = distr_id,
        param_id = param_id
      )
      distr_id <- distr_id + 1
      param_id <- param_id + get_distr_n_params( # nolint beautier function
        tn93_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    }
  }

  # kappa_1_prior_distr
  if (!is_init_distr(tn93_site_model$kappa_1_prior_distr)) { # nolint beautier function
    tn93_site_model$kappa_1_prior_distr <- init_distr( # nolint beautier function
      tn93_site_model$kappa_1_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + get_distr_n_params( # nolint beautier function
      tn93_site_model$kappa_1_prior_distr
    )
  }

  # kappa_2_prior_distr
  if (!is_init_distr(tn93_site_model$kappa_2_prior_distr)) { # nolint beautier function
    tn93_site_model$kappa_2_prior_distr <- init_distr( # nolint beautier function
      tn93_site_model$kappa_2_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + get_distr_n_params( # nolint beautier function
      tn93_site_model$kappa_2_prior_distr
    )
  }

  if (!is_init_param(tn93_site_model$kappa_1_param)) { # nolint beautier function
    tn93_site_model$kappa_1_param <- init_param( # nolint beautier function
      tn93_site_model$kappa_1_param,
      id = param_id
    )
    param_id <- param_id + 1
  }

  if (!is_init_param(tn93_site_model$kappa_2_param)) { # nolint beautier function
    tn93_site_model$kappa_2_param <- init_param( # nolint beautier function
      tn93_site_model$kappa_2_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  testit::assert(is_init_gamma_site_model(tn93_site_model$gamma_site_model)) # nolint beautier function
  testit::assert(is_init_tn93_site_model(tn93_site_model)) # nolint beautier function
  tn93_site_model
}
