#' Initializes all site models
#' @inheritParams default_params_doc
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized site models
#' @author Richèl J.C. Bilderbeek
#' @export
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
      site_model <- beautier::init_gtr_site_model(
        site_model,
        distr_id = distr_id,
        param_id = param_id
      )
    } else if (beautier::is_hky_site_model(site_model)) {
      # HKY
      site_model <- beautier::init_hky_site_model(
        site_model,
        distr_id = distr_id,
        param_id = param_id
      )
    } else if (beautier::is_jc69_site_model(site_model)) {
      # JC69
      site_model <- beautier::init_jc69_site_model(
        site_model,
        distr_id = distr_id,
        param_id = param_id
      )
    } else {
      testit::assert(beautier::is_tn93_site_model(site_model))
      site_model <- beautier::init_tn93_site_model(
        site_model,
        distr_id = distr_id,
        param_id = param_id
      )
    }

    distr_id <- distr_id + beautier::get_site_model_n_distrs(site_model)
    param_id <- param_id + beautier::get_site_model_n_params(site_model)

    if (beautier::is_one_na(site_model$id)) site_model$id <- ids[i]
    testit::assert(beautier::is_init_site_model(site_model))
    site_models[[i]] <- site_model
  }
  site_models
}

#' Initializes a GTR site model
#' @inheritParams default_params_doc
#' @return an initialized GTR site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' gtr_site_model <- create_gtr_site_model()
#' # FALSE
#' is_init_gtr_site_model(gtr_site_model)
#' gtr_site_model <- init_gtr_site_model(gtr_site_model)
#' # TRUE
#' is_init_gtr_site_model(gtr_site_model)
#'
#' check_empty_beautier_folder()
#' @export
init_gtr_site_model <- function(
  gtr_site_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_gtr_site_model(gtr_site_model))

  # Initialize gamma site model, if any
  if (
    !beautier::is_one_na(
      gtr_site_model$gamma_site_model$gamma_shape_prior_distr
    )
  ) {
    if (
      !beautier::is_init_distr(
        gtr_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    ) {
      gtr_site_model$gamma_site_model$gamma_shape_prior_distr <-
        beautier::init_distr(
          gtr_site_model$gamma_site_model$gamma_shape_prior_distr,
          distr_id = distr_id,
          param_id = param_id
        )
      distr_id <- distr_id + 1
      param_id <- param_id + beautier::get_distr_n_params(
        gtr_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    }
  }

  if (!beautier::is_init_distr(gtr_site_model$rate_ac_prior_distr)) {
    gtr_site_model$rate_ac_prior_distr <- beautier::init_distr(
      gtr_site_model$rate_ac_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(
      gtr_site_model$rate_ac_prior_distr
    )
  }
  if (!beautier::is_init_distr(gtr_site_model$rate_ag_prior_distr)) {
    gtr_site_model$rate_ag_prior_distr <- beautier::init_distr(
      gtr_site_model$rate_ag_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(
      gtr_site_model$rate_ag_prior_distr
    )
  }
  if (!beautier::is_init_distr(gtr_site_model$rate_at_prior_distr)) {
    gtr_site_model$rate_at_prior_distr <- beautier::init_distr(
      gtr_site_model$rate_at_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(
      gtr_site_model$rate_at_prior_distr
    )
  }
  if (!beautier::is_init_distr(gtr_site_model$rate_cg_prior_distr)) {
    gtr_site_model$rate_cg_prior_distr <- beautier::init_distr(
      gtr_site_model$rate_cg_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(
      gtr_site_model$rate_cg_prior_distr
    )
  }
  if (!beautier::is_init_distr(gtr_site_model$rate_gt_prior_distr)) {
    gtr_site_model$rate_gt_prior_distr <- beautier::init_distr(
      gtr_site_model$rate_gt_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(
      gtr_site_model$rate_gt_prior_distr
    )
  }
  if (!beautier::is_init_param(gtr_site_model$rate_ac_param)) {
    gtr_site_model$rate_ac_param <- beautier::init_param(
      gtr_site_model$rate_ac_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  if (!beautier::is_init_param(gtr_site_model$rate_ag_param)) {
    gtr_site_model$rate_ag_param <- beautier::init_param(
      gtr_site_model$rate_ag_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  if (!beautier::is_init_param(gtr_site_model$rate_at_param)) {
    gtr_site_model$rate_at_param <- beautier::init_param(
      gtr_site_model$rate_at_param, id = param_id
    )
    param_id <- param_id + 1
  }
  if (!beautier::is_init_param(gtr_site_model$rate_cg_param)) {
    gtr_site_model$rate_cg_param <- beautier::init_param(
      gtr_site_model$rate_cg_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  if (!beautier::is_init_param(gtr_site_model$rate_ct_param)) {
    gtr_site_model$rate_ct_param <- beautier::init_param(
      gtr_site_model$rate_ct_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  if (!beautier::is_init_param(gtr_site_model$rate_gt_param)) {
    gtr_site_model$rate_gt_param <- beautier::init_param(
      gtr_site_model$rate_gt_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  testit::assert(beautier::is_gtr_site_model(gtr_site_model))
  testit::assert(
    beautier::is_init_gamma_site_model(gtr_site_model$gamma_site_model)
  )
  testit::assert(beautier::is_init_gtr_site_model(gtr_site_model))
  gtr_site_model
}

#' Initializes an HKY site model
#' @inheritParams default_params_doc
#' @return an initialized HKY site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' hky_site_model <- create_hky_site_model()
#' is_init_hky_site_model(hky_site_model)
#' hky_site_model <- init_hky_site_model(hky_site_model)
#' is_init_hky_site_model(hky_site_model)
#'
#' check_empty_beautier_folder()
#' @export
init_hky_site_model <- function(
  hky_site_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_hky_site_model(hky_site_model))

  # Initialize gamma site model, if any
  if (
    !beautier::is_one_na(
      hky_site_model$gamma_site_model$gamma_shape_prior_distr
    )
  ) {
    if (!beautier::is_init_distr(
      hky_site_model$gamma_site_model$gamma_shape_prior_distr
    )) {
      hky_site_model$gamma_site_model$gamma_shape_prior_distr <-
        beautier::init_distr(
          hky_site_model$gamma_site_model$gamma_shape_prior_distr,
          distr_id = distr_id,
          param_id = param_id
        )
      distr_id <- distr_id + 1
      param_id <- param_id + beautier::get_distr_n_params(
        hky_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    }
  }

  # kappa_prior_distr
  if (!beautier::is_init_distr(hky_site_model$kappa_prior_distr)) {
    hky_site_model$kappa_prior_distr <- beautier::init_distr(
      hky_site_model$kappa_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(
      hky_site_model$kappa_prior_distr
    )
  }

  testit::assert(
    beautier::is_init_gamma_site_model(hky_site_model$gamma_site_model)
  )
  testit::assert(beautier::is_init_hky_site_model(hky_site_model))
  hky_site_model
}


#' Initializes a JC69 site model
#' @inheritParams default_params_doc
#' @return an initialized HKY site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' hky_site_model <- create_hky_site_model()
#' is_init_hky_site_model(hky_site_model)
#' hky_site_model <- init_hky_site_model(hky_site_model)
#' is_init_hky_site_model(hky_site_model)
#'
#' check_empty_beautier_folder()
#' @export
init_jc69_site_model <- function(
  jc69_site_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_jc69_site_model(jc69_site_model))

  # Initialize gamma site model, if any
  if (
    !beautier::is_one_na(
      jc69_site_model$gamma_site_model$gamma_shape_prior_distr
    )
  ) {
    if (
      !beautier::is_init_distr(
        jc69_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    ) {
      jc69_site_model$gamma_site_model$gamma_shape_prior_distr <-
        beautier::init_distr(
          jc69_site_model$gamma_site_model$gamma_shape_prior_distr,
          distr_id = distr_id,
          param_id = param_id
        )
      distr_id <- distr_id + 1
      param_id <- param_id + beautier::get_distr_n_params(
        jc69_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    }
  }

  testit::assert(
    beautier::is_init_gamma_site_model(jc69_site_model$gamma_site_model)
  )
  testit::assert(beautier::is_init_jc69_site_model(jc69_site_model))
  jc69_site_model
}



#' Initializes a TN93 site model
#' @inheritParams default_params_doc
#' @return an initialized TN93 site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' tn93_site_model <- create_tn93_site_model()
#' is_init_tn93_site_model(tn93_site_model)
#' tn93_site_model <- init_tn93_site_model(tn93_site_model)
#' is_init_tn93_site_model(tn93_site_model)
#'
#' check_empty_beautier_folder()
#' @export
init_tn93_site_model <- function(
  tn93_site_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_tn93_site_model(tn93_site_model))

  # Initialize gamma site model, if any
  if (
    !beautier::is_one_na(
      tn93_site_model$gamma_site_model$gamma_shape_prior_distr
    )
  ) {
    if (
      !beautier::is_init_distr(
        tn93_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    ) {
      tn93_site_model$gamma_site_model$gamma_shape_prior_distr <-
        beautier::init_distr(
          tn93_site_model$gamma_site_model$gamma_shape_prior_distr,
          distr_id = distr_id,
          param_id = param_id
        )
      distr_id <- distr_id + 1
      param_id <- param_id + beautier::get_distr_n_params(
        tn93_site_model$gamma_site_model$gamma_shape_prior_distr
      )
    }
  }

  # kappa_1_prior_distr
  if (!beautier::is_init_distr(tn93_site_model$kappa_1_prior_distr)) {
    tn93_site_model$kappa_1_prior_distr <- beautier::init_distr(
      tn93_site_model$kappa_1_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(
      tn93_site_model$kappa_1_prior_distr
    )
  }

  # kappa_2_prior_distr
  if (!beautier::is_init_distr(tn93_site_model$kappa_2_prior_distr)) {
    tn93_site_model$kappa_2_prior_distr <- beautier::init_distr(
      tn93_site_model$kappa_2_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(
      tn93_site_model$kappa_2_prior_distr
    )
  }

  if (!beautier::is_init_param(tn93_site_model$kappa_1_param)) {
    tn93_site_model$kappa_1_param <- beautier::init_param(
      tn93_site_model$kappa_1_param,
      id = param_id
    )
    param_id <- param_id + 1
  }

  if (!beautier::is_init_param(tn93_site_model$kappa_2_param)) {
    tn93_site_model$kappa_2_param <- beautier::init_param(
      tn93_site_model$kappa_2_param,
      id = param_id
    )
    param_id <- param_id + 1
  }
  testit::assert(
    beautier::is_init_gamma_site_model(tn93_site_model$gamma_site_model)
  )
  testit::assert(beautier::is_init_tn93_site_model(tn93_site_model))
  tn93_site_model
}
