library(beautier)

create_random_estimate <- function() {
  sample(x = 1:2, size = 1) == 1
}

create_random_alpha_param <- function() {
  create_alpha_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100)
  )
}

create_random_beta_param <- function() {
  create_beta_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100)
  )
}

create_random_clock_rate_param <- function() {
  create_clock_rate_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100)
  )
}

create_random_kappa_1_param <- function() {
  create_kappa_1_param(
    lower = runif(n = 1, min = -100, max = 100),
    value = runif(n = 1, min = -100, max = 100)
  )
}

create_random_kappa_2_param <- function() {
  create_kappa_2_param(
    lower = runif(n = 1, min = -100, max = 100),
    value = runif(n = 1, min = -100, max = 100)
  )
}

create_random_lambda_param <- function() {
  create_lambda_param(
    value = runif(n = 1, min = -100, max = 100)
  )
}

create_random_m_param <- function() {
  create_m_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100)
  )
}

create_random_mean_param <- function() {
  create_mean_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100)
  )
}

create_random_mu_param <- function() {
  create_mu_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100)
  )
}

create_random_rate_ac_param <- function() {
  create_rate_ac_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100),
    lower = runif(n = 1, min = -100, max = 100)
  )
}

create_random_rate_ag_param <- function() {
  create_rate_ag_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100),
    lower = runif(n = 1, min = -100, max = 100)
  )
}

create_random_rate_at_param <- function() {
  create_rate_at_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100),
    lower = runif(n = 1, min = -100, max = 100)
  )
}

create_random_rate_cg_param <- function() {
  create_rate_cg_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100),
    lower = runif(n = 1, min = -100, max = 100)
  )

}

create_random_rate_ct_param <- function() {
  create_rate_ct_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100),
    lower = runif(n = 1, min = -100, max = 100)
  )
}

create_random_rate_gt_param <- function() {
  create_rate_gt_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100),
    lower = runif(n = 1, min = -100, max = 100)
  )

}
create_random_s_param <- function() {
  create_s_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100),
    lower = runif(n = 1, min = -100, max = 100)
  )
}

create_random_scale_param <- function() {
  create_scale_param(
    estimate = create_random_estimate(),
    value = runif(n = 1, min = -100, max = 100)
  )
}

create_random_sigma_param <- function() {
  sigma_param <- NA
  while (length(sigma_param) == 1 && is.na(sigma_param)) {
    tryCatch(
      sigma_param <- create_sigma_param(
        estimate = create_random_estimate(),
        value = runif(n = 1, min = -100, max = 100)
      ),
      error = function(cond) {}
    )
  }
  sigma_param
}



create_random_beta_distr <- function() {
  create_beta_distr(
    alpha = create_random_alpha_param(),
    beta = create_random_beta_param()
  )
}

create_random_exp_distr <- function() {
  create_exp_distr(
    mean = create_random_mean_param()
  )
}

create_random_gamma_distr <- function() {
  create_gamma_distr(
    alpha = create_random_alpha_param(),
    beta = create_random_beta_param()
  )
}

create_random_inv_gamma_distr <- function() {
  create_inv_gamma_distr(
    alpha = create_random_alpha_param(),
    beta = create_random_beta_param()
  )
}

create_random_laplace_distr <- function() {
  create_laplace_distr(
    mu = create_random_mu_param(),
    scale = create_random_scale_param()
  )
}

create_random_log_normal_distr <- function() {
  create_log_normal_distr(
    m = create_random_m_param(),
    s = create_random_s_param()
  )
}

create_random_normal_distr <- function() {
  create_normal_distr(
    mean = create_random_mean_param(),
    sigma = create_random_sigma_param()
  )
}

create_random_one_div_x_distr <- function() {
  create_one_div_x_distr()
}

create_random_poisson_distr <- function() {
  create_poisson_distr(
    lambda = create_random_lambda_param()
  )
}

create_random_uniform_distr <- function() {

  uniform_distr <- NA
  while (length(uniform_distr) == 1 && is.na(uniform_distr)) {
    tryCatch(
      uniform_distr <- create_uniform_distr(
        upper = runif(n = 1, min = -100, max = 100)
      ),
      error = function(cond) {}
    )
  }
  uniform_distr
}


create_random_freq_equilibrium <- function() {
  options <- c("estimated", "empirical", "all_equal")
  options[ sample(x = 1:3, size = 1) ]
}

create_random_distr <- function() {
  distr_index <- sample(x = 1:10, size = 1)
  if (distr_index == 1) {
    create_random_beta_distr()
  } else if (distr_index == 2) {
    create_random_exp_distr()
  } else if (distr_index == 3) {
    create_random_gamma_distr()
  } else if (distr_index == 4) {
    create_random_inv_gamma_distr()
  } else if (distr_index == 5) {
    create_random_laplace_distr()
  } else if (distr_index == 6) {
    create_random_log_normal_distr()
  } else if (distr_index == 7) {
    create_random_normal_distr()
  } else if (distr_index == 8) {
    create_random_one_div_x_distr()
  } else if (distr_index == 9) {
    create_random_poisson_distr()
  } else if (distr_index == 10) {
    create_random_uniform_distr()
  } else {
    testit::assert(!"Should not get here")
  }
}


create_random_gamma_site_model <- function() {
  gamma_site_model <- create_gamma_site_model(
    gamma_cat_count = sample(x = 0:4, size = 1),
    gamma_shape = runif(n = 1, min = 0.0, max = 1.0),
    prop_invariant = runif(n = 1, min = 0.0, max = 1.0),
    gamma_shape_prior_distr = create_random_distr(),
    freq_equilibrium = create_random_freq_equilibrium()
  )
  testit::assert(beautier:::is_gamma_site_model(gamma_site_model))
  gamma_site_model
}

create_random_jc69_site_model <- function() {
  create_jc69_site_model(
    gamma_site_model = create_random_gamma_site_model()
  )
}


create_random_site_model <- function() {
  create_random_jc69_site_model()
}


create_random <- function(
  input_fasta_filename = beautier:::get_path("anthus_aco.fas")
) {

  input_fasta_filename <- beautier:::get_path("anthus_aco.fas")
  site_model <- create_random_site_model()
  clock_model <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()

  output_xml_filename <- tempfile()
  create_beast2_input_file(
    input_filenames = input_fasta_filename,
    output_filename = output_xml_filename,
    site_models = site_model,
    clock_models = clock_model,
    tree_priors = tree_prior
  )
  is_ok <- beastier::is_beast2_input_file(output_xml_filename)
  if (!is_ok) {
    print("ERROR")
    file.copy(output_xml_filename, "/home/richel/bad.xml")
    beastier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
    print("site model:")
    print(site_model)
    print("clock model:")
    print(clock_model)
    print("tree prior:")
    print(tree_prior)
  }
  is_ok
}

set.seed(0)
create_random()

status <- 0
for (i in seq(1, 100)) {
  print("--------------------")
  print(paste("-",i,"-"))
  print("--------------------")
  ok <- create_random()
  if (ok == FALSE) {
    status <- 1
    break
  }
}

# quit(status = status, save = "no")
