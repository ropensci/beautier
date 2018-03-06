#' Create a random alpha parameter
#' @author Richel J.C. Bilderbeek
create_random_alpha_param <- function() {
  create_alpha_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random BD tree prior
#' @author Richel J.C. Bilderbeek
create_random_bd_tree_prior <- function() {
  create_bd_tree_prior(
    birth_rate_distr = create_random_distr(), # nolint internal function
    death_rate_distr = create_random_distr() # nolint internal function
  )
}

#' Create a random beta distribution
#' @author Richel J.C. Bilderbeek
create_random_beta_distr <- function() {

  beta_distr <- NA
  while (length(beta_distr) == 1 && is.na(beta_distr)) {
    tryCatch(
      beta_distr <- create_beta_distr(
        alpha = create_random_alpha_param(), # nolint internal function
        beta = create_random_beta_param() # nolint internal function
      ),
      error = function(cond) {} # nolint
    )
  }
  beta_distr
}

#' Create a random beta parameter
#' @author Richel J.C. Bilderbeek
create_random_beta_param <- function() {
  create_beta_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random boolean
#' @author Richel J.C. Bilderbeek
create_random_bool <- function() {
  sample(x = 1:2, size = 1) == 1
}

#' Create a random CBS tree prior
#' @author Richel J.C. Bilderbeek
create_random_cbs_tree_prior <- function() {
  create_cbs_tree_prior()
}

#' Create a random CCP tree prior
#' @author Richel J.C. Bilderbeek
create_random_ccp_tree_prior <- function() {
  create_ccp_tree_prior(
    pop_size_distr = create_random_distr() # nolint internal function
  )
}

#' Create a random CEP tree prior
#' @author Richel J.C. Bilderbeek
create_random_cep_tree_prior <- function() {
  create_cep_tree_prior(
    pop_size_distr = create_random_distr(), # nolint internal function
    growth_rate_distr = create_random_distr() # nolint internal function
  )
}

#' Create a random clock model
#' @author Richel J.C. Bilderbeek
create_random_clock_model <- function() {
  clock_model_index <- sample(x = 1:2, size = 1)
  if (clock_model_index == 1) {
    create_random_rln_clock_model()
  } else if (clock_model_index == 2) {
    create_random_strict_clock_model()
  } else {
    testit::assert(!"Should not get here")
  }
}

#' Create a random clock rate parameter
#' @author Richel J.C. Bilderbeek
create_random_clock_rate_param <- function() {
  create_clock_rate_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random distribution
#' @author Richel J.C. Bilderbeek
create_random_distr <- function() {
  distr_index <- sample(x = 1:10, size = 1)
  if (distr_index == 1) {
    create_random_beta_distr() # nolint internal function
  } else if (distr_index == 2) {
    create_random_exp_distr() # nolint internal function
  } else if (distr_index == 3) {
    create_random_gamma_distr() # nolint internal function
  } else if (distr_index == 4) {
    create_random_inv_gamma_distr() # nolint internal function
  } else if (distr_index == 5) {
    create_random_laplace_distr() # nolint internal function
  } else if (distr_index == 6) {
    create_random_log_normal_distr() # nolint internal function
  } else if (distr_index == 7) {
    create_random_normal_distr() # nolint internal function
  } else if (distr_index == 8) {
    create_random_one_div_x_distr() # nolint internal function
  } else if (distr_index == 9) {
    create_random_poisson_distr() # nolint internal function
  } else if (distr_index == 10) {
    create_random_uniform_distr() # nolint internal function
  } else {
    testit::assert(!"Should not get here")
  }
}

#' Create a random value for 'estimate',
#' which must be TRUE or FALSE
#' @author Richel J.C. Bilderbeek
create_random_estimate <- function() {
  create_random_bool() # nolint internal function
}

#' Create a random exponential distribution
#' @author Richel J.C. Bilderbeek
create_random_exp_distr <- function() {
  create_exp_distr(
    mean = create_random_mean_param() # nolint internal function
  )
}

#' Create a random value for 'freq equilibrium', which
#' can be 'estimated', 'empirical' or 'all_equal'
#' @author Richel J.C. Bilderbeek
create_random_freq_equilibrium <- function() {
  options <- c("estimated", "empirical", "all_equal")
  options[ sample(x = 1:3, size = 1) ]
}

#' Create a random gamma distribution
#' @author Richel J.C. Bilderbeek
create_random_gamma_distr <- function() {

  gamma_distr <- NA
  while (length(gamma_distr) == 1 && is.na(gamma_distr)) {
    tryCatch(
      gamma_distr <- create_gamma_distr(
      alpha = create_random_alpha_param(), # nolint internal function
      beta = create_random_beta_param() # nolint internal function
    ),
      error = function(cond) {} # nolint
    )
  }
  gamma_distr
}

#' Create a random gamma site model
#' @author Richel J.C. Bilderbeek
create_random_gamma_site_model <- function() {
  gamma_site_model <- NA
  while (length(gamma_site_model) == 1 && is.na(gamma_site_model)) {
    tryCatch(
      gamma_site_model <- create_gamma_site_model(
        gamma_cat_count = sample(x = -1:4, size = 1),
        gamma_shape = stats::runif(n = 1, min = -1.0, max = 1.0),
        prop_invariant = stats::runif(n = 1, min = -1.0, max = 1.0),
        gamma_shape_prior_distr = create_random_distr(), # nolint internal function
        freq_equilibrium = create_random_freq_equilibrium() # nolint internal function
      ),
      error = function(cond) {} # nolint
    )
  }
  testit::assert(is_gamma_site_model(gamma_site_model)) # nolint internal function
  gamma_site_model
}

#' Create a random GTR site model
#' @author Richel J.C. Bilderbeek
create_random_gtr_site_model <- function() {
  create_gtr_site_model(
    gamma_site_model = create_random_gamma_site_model(), # nolint internal function
    rate_ac_prior_distr = create_random_distr(), # nolint internal function
    rate_ag_prior_distr = create_random_distr(), # nolint internal function
    rate_at_prior_distr = create_random_distr(), # nolint internal function
    rate_cg_prior_distr = create_random_distr(), # nolint internal function
    rate_gt_prior_distr = create_random_distr(), # nolint internal function
    rate_ac_param = create_random_rate_ac_param(), # nolint internal function
    rate_ag_param = create_random_rate_ag_param(), # nolint internal function
    rate_at_param = create_random_rate_at_param(), # nolint internal function
    rate_cg_param = create_random_rate_cg_param(), # nolint internal function
    rate_ct_param = create_random_rate_ct_param(), # nolint internal function
    rate_gt_param = create_random_rate_gt_param(), # nolint internal function
    freq_equilibrium = create_random_freq_equilibrium() # nolint internal function
  )
}

#' Create a random HKY site model
#' @author Richel J.C. Bilderbeek
create_random_hky_site_model <- function() {
  create_hky_site_model(
    gamma_site_model = create_random_gamma_site_model(), # nolint internal function
    kappa = stats::runif(n = 1, min = -100.0, max = 100.0),
    kappa_prior_distr = create_random_distr(), # nolint internal function
    freq_equilibrium = create_random_freq_equilibrium() # nolint internal function
  )
}

#' Create a random inverse-gamma distribution
#' @author Richel J.C. Bilderbeek
create_random_inv_gamma_distr <- function() {
  create_inv_gamma_distr(
    alpha = create_random_alpha_param(), # nolint internal function
    beta = create_random_beta_param() # nolint internal function
  )
}

#' Create a random JC69 distribution
#' @author Richel J.C. Bilderbeek
create_random_jc69_site_model <- function() {
  create_jc69_site_model(
    gamma_site_model = create_random_gamma_site_model() # nolint internal function
  )
}

#' Create a random kappa 1 parameter
#' @author Richel J.C. Bilderbeek
create_random_kappa_1_param <- function() {
  create_kappa_1_param(
    lower = stats::runif(n = 1, min = -100, max = 100),
    value = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random kappa 2 parameter
#' @author Richel J.C. Bilderbeek
create_random_kappa_2_param <- function() {
  create_kappa_2_param(
    lower = stats::runif(n = 1, min = -100, max = 100),
    value = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random lambda parameter
#' @author Richel J.C. Bilderbeek
create_random_lambda_param <- function() {
  create_lambda_param(
    value = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random Laplace distribution
#' @author Richel J.C. Bilderbeek
create_random_laplace_distr <- function() {
  create_laplace_distr(
    mu = create_random_mu_param(), # nolint internal function
    scale = create_random_scale_param() # nolint internal function
  )
}

#' Create a random log-normal distribution
#' @author Richel J.C. Bilderbeek
create_random_log_normal_distr <- function() {

  log_normal_distr <- NA
  while (length(log_normal_distr) == 1 && is.na(log_normal_distr)) {
    tryCatch(
      log_normal_distr <- create_log_normal_distr(
        m = create_random_m_param(), # nolint internal function
        s = create_random_s_param() # nolint internal function
      ),
      error = function(cond) {} # nolint
    )
  }
  log_normal_distr
}

#' Create a random m parameter
#' @author Richel J.C. Bilderbeek
create_random_m_param <- function() {
  create_m_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random mean parameter
#' @author Richel J.C. Bilderbeek
create_random_mean_param <- function() {
  create_mean_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random mu parameter
#' @author Richel J.C. Bilderbeek
create_random_mu_param <- function() {
  create_mu_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random normal distribution
#' @author Richel J.C. Bilderbeek
create_random_normal_distr <- function() {
  create_normal_distr(
    mean = create_random_mean_param(), # nolint internal function
    sigma = create_random_sigma_param() # nolint internal function
  )
}

#' Create a random 1/x distribution
#' @author Richel J.C. Bilderbeek
create_random_one_div_x_distr <- function() {
  create_one_div_x_distr()
}

#' Create a random Poisson distribution
#' @author Richel J.C. Bilderbeek
create_random_poisson_distr <- function() {
  create_poisson_distr(
    lambda = create_random_lambda_param() # nolint internal function
  )
}

#' Create a random rate AC parameter
#' @author Richel J.C. Bilderbeek
create_random_rate_ac_param <- function() {
  create_rate_ac_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100),
    lower = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random rate AG parameter
#' @author Richel J.C. Bilderbeek
create_random_rate_ag_param <- function() {
  create_rate_ag_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100),
    lower = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random rate AT parameter
#' @author Richel J.C. Bilderbeek
create_random_rate_at_param <- function() {
  create_rate_at_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100),
    lower = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random rate CG parameter
#' @author Richel J.C. Bilderbeek
create_random_rate_cg_param <- function() {
  create_rate_cg_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100),
    lower = stats::runif(n = 1, min = -100, max = 100)
  )

}

#' Create a random rate CT parameter
#' @author Richel J.C. Bilderbeek
create_random_rate_ct_param <- function() {
  create_rate_ct_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100),
    lower = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random rate GT parameter
#' @author Richel J.C. Bilderbeek
create_random_rate_gt_param <- function() {
  create_rate_gt_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100),
    lower = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random RLN clock model
#' @author Richel J.C. Bilderbeek
create_random_rln_clock_model <- function() {

  rln_clock_model <- NA
  while (length(rln_clock_model) == 1 && is.na(rln_clock_model)) {
    tryCatch(
      rln_clock_model <- create_rln_clock_model(
        mean_rate_prior_distr = create_random_distr(), # nolint internal function
        ucldstdev_distr = create_random_distr(), # nolint internal function
        mean_clock_rate = stats::runif(n = 1, min = -100.0, max = 100.0),
        n_rate_categories = sample(x = -2:10, size = 1),
        normalize_mean_clock_rate = create_random_bool() # nolint internal function
      ),
      error = function(cond) {} # nolint
    )
  }
  rln_clock_model
}

#' Create a random s parameter
#' @author Richel J.C. Bilderbeek
create_random_s_param <- function() {
  create_s_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100),
    lower = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random scale parameter
#' @author Richel J.C. Bilderbeek
create_random_scale_param <- function() {
  create_scale_param(
    estimate = create_random_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -100, max = 100)
  )
}

#' Create a random sigma parameter
#' @author Richel J.C. Bilderbeek
create_random_sigma_param <- function() {
  sigma_param <- NA
  while (length(sigma_param) == 1 && is.na(sigma_param)) {
    tryCatch(
      sigma_param <- create_sigma_param(
        estimate = create_random_estimate(), # nolint internal function
        value = stats::runif(n = 1, min = -100, max = 100)
      ),
      error = function(cond) {} # nolint
    )
  }
  sigma_param
}

#' Create a random site model
#' @author Richel J.C. Bilderbeek
create_random_site_model <- function() {

  site_model_index <- sample(x = 1:4, size = 1)
  if (site_model_index == 1) {
    create_random_jc69_site_model() # nolint internal function
  } else if (site_model_index == 2) {
    create_random_hky_site_model() # nolint internal function
  } else if (site_model_index == 3) {
    create_random_tn93_site_model() # nolint internal function
  } else if (site_model_index == 4) {
    create_random_gtr_site_model() # nolint internal function
  } else {
    testit::assert(!"Should not get here")
  }
}

#' Create a random strict clock model
#' @author Richel J.C. Bilderbeek
create_random_strict_clock_model <- function() {

  strict_clock_model <- NA
  while (length(strict_clock_model) == 1 && is.na(strict_clock_model)) {
    tryCatch(
      strict_clock_model <- create_strict_clock_model(
        clock_rate_param = create_random_clock_rate_param(), # nolint internal function
        clock_rate_distr = create_random_distr() # nolint internal function
      ),
      error = function(cond) {} # nolint
    )
  }
  strict_clock_model
}

#' Create a random TN93 site model
#' @author Richel J.C. Bilderbeek
create_random_tn93_site_model <- function() {
  create_tn93_site_model(
    gamma_site_model = create_random_gamma_site_model(), # nolint internal function
    kappa_1_param = create_random_kappa_1_param(), # nolint internal function
    kappa_2_param = create_random_kappa_2_param(), # nolint internal function
    kappa_1_prior_distr = create_random_distr(), # nolint internal function
    kappa_2_prior_distr = create_random_distr(), # nolint internal function
    freq_equilibrium = create_random_freq_equilibrium() # nolint internal function
  )
}

#' Create a random tree prior
#' @author Richel J.C. Bilderbeek
create_random_tree_prior <- function() {
  tree_prior_index <- sample(x = 1:5, size = 1)

  if (tree_prior_index == 1) {
    create_random_bd_tree_prior() # nolint internal function
  } else if (tree_prior_index == 2) {
    create_random_cbs_tree_prior() # nolint internal function
  } else if (tree_prior_index == 3) {
    create_random_ccp_tree_prior() # nolint internal function
  } else if (tree_prior_index == 4) {
    create_random_cep_tree_prior() # nolint internal function
  } else if (tree_prior_index == 5) {
    create_random_yule_tree_prior() # nolint internal function
  } else {
    testit::assert(!"Should not get here")
  }
}

#' Create a random uniform distribution
#' @author Richel J.C. Bilderbeek
create_random_uniform_distr <- function() {

  uniform_distr <- NA
  while (length(uniform_distr) == 1 && is.na(uniform_distr)) {
    tryCatch(
      uniform_distr <- create_uniform_distr(
        upper = stats::runif(n = 1, min = -100, max = 100)
      ),
      error = function(cond) {} # nolint
    )
  }
  uniform_distr
}

#' Create a random Yule tree prior
#' @author Richel J.C. Bilderbeek
create_random_yule_tree_prior <- function() {
  create_yule_tree_prior(
    birth_rate_distr = create_random_distr() # nolint internal function
  )
}
