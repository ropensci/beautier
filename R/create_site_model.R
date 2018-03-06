#' General function to create a site model.
#' @param name the site model name. Valid
#'   names can be found in \code{\link{get_site_model_names}}
#' @param id the IDs of the alignment (can be extracted from
#'   the FASTA filesname using \code{\link{get_id}})
#' @param gamma_site_model a gamma site model, as created
#'   by \code{\link{create_gamma_site_model}}
#' @param ... specific site model parameters
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_gtr_site_model}},
#'   \code{\link{create_hky_site_model}},,
#'   \code{\link{create_jc69_site_model}},
#'   and \code{\link{create_tn93_site_model}}
#' @seealso See \code{\link{create_gtr_site_model}} for more examples
#'   with a GTR site model. See \code{\link{create_hky_site_model}}
#'   for more examples with an HKY site model. See
#'   \code{\link{create_jc69_site_model}} for more examples with a JC69
#'   site model. See \code{\link{create_tn93_site_model}} for more
#'   examples with a TN93 site model
#' @return a site_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # GTR
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     output_filename = "example_gtr.xml",
#'     site_models = create_gtr_site_model()
#'   )
#'   testthat::expect_true(file.exists("example_gtr.xml"))
#'
#'   # HKY
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     output_filename = "example_hky.xml",
#'     site_models = create_hky_site_model()
#'   )
#'   testthat::expect_true(file.exists("example_hky.xml"))
#'
#'   # JC69
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     output_filename = "example_jc69.xml",
#'     site_models = create_jc69_site_model()
#'   )
#'   testthat::expect_true(file.exists("example_jc69.xml"))
#'
#'   # TN93
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     output_filename = "example_tn93.xml",
#'     site_models = create_tn93_site_model()
#'   )
#'   testthat::expect_true(file.exists("example_tn93.xml"))
#' @export
create_site_model <- function(
  name,
  id,
  gamma_site_model = create_gamma_site_model(),
  ...
) {
  if (!is_site_model_name(name)) {
    site_models_as_string <- function() {
      s <- NULL
      for (p in get_site_model_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "'site model' must be a site model name, which is one of these: ",
      site_models_as_string()
    )
  }
  if (!is_gamma_site_model(gamma_site_model)) {
    stop("'gamma_site_model' must be a valid gamma site model")
  }
  site_model <- list(
    name = name,
    id = id,
    gamma_site_model = gamma_site_model,
    ...
  )
  site_model
}

#' Create a GTR site model
#' @inheritParams create_site_model
#' @param rate_ac_prior_distr the AC rate prior distribution,
#'   as returned by \code{\link{create_distr}})
#' @param rate_ag_prior_distr the AG rate prior distribution,
#'   as returned by \code{\link{create_distr}})
#' @param rate_at_prior_distr the AT rate prior distribution,
#'   as returned by \code{\link{create_distr}})
#' @param rate_cg_prior_distr the CG rate prior distribution,
#'   as returned by \code{\link{create_distr}})
#' @param rate_gt_prior_distr the GT rate prior distribution,
#'   as returned by \code{\link{create_distr}})
#' @param rate_ac_param the 'rate AC' parameter,
#'   as returned by \code{\link{create_rate_ac_param}})
#' @param rate_ag_param the 'rate AG' parameter,
#'   as returned by \code{\link{create_rate_ag_param}})
#' @param rate_at_param the 'rate AT' parameter,
#'   as returned by \code{\link{create_rate_at_param}})
#' @param rate_cg_param the 'rate CG' parameter,
#'   as returned by \code{\link{create_rate_cg_param}})
#' @param rate_ct_param the 'rate CT' parameter,
#'   as returned by \code{\link{create_rate_ct_param}})
#' @param rate_gt_param the 'rate GT' parameter,
#'   as returned by \code{\link{create_rate_gt_param}})
#' @param freq_equilibrium the frequency in which the rates are at equilibrium
#'   are either \code{estimated}, \code{empirical} or \code{all_equal}.
#'   \code{\link{get_freq_equilibrium_names}} returns the possible values
#'   for \code{freq_equilibrium}
#' @return a GTR site_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   gtr_site_model <- create_gtr_site_model()
#'
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "create_gtr_site_model.xml",
#'     site_models = gtr_site_model
#'   )
#'   testit::assert(file.exists("create_gtr_site_model.xml"))
#' @aliases create_gtr_site_model create_site_model_gtr
#' @export create_gtr_site_model create_site_model_gtr
create_gtr_site_model <- create_site_model_gtr <- function(
  id = NA,
  gamma_site_model = create_gamma_site_model(),
  rate_ac_prior_distr = create_gamma_distr(
    alpha = create_alpha_param(value = "0.05"),
    beta = create_beta_param(value = "10.0")
  ),
  rate_ag_prior_distr = create_gamma_distr(
    alpha = create_alpha_param(value = "0.05"),
    beta = create_beta_param(value = "20.0")
  ),
  rate_at_prior_distr = create_gamma_distr(
    alpha = create_alpha_param(value = "0.05"),
    beta = create_beta_param(value = "10.0")
  ),
  rate_cg_prior_distr = create_gamma_distr(
    alpha = create_alpha_param(value = "0.05"),
    beta = create_beta_param(value = "10.0")
  ),
  rate_gt_prior_distr = create_gamma_distr(
    alpha = create_alpha_param(value = "0.05"),
    beta = create_beta_param(value = "10.0")
  ),
  rate_ac_param = create_rate_ac_param(),
  rate_ag_param = create_rate_ag_param(),
  rate_at_param = create_rate_at_param(),
  rate_cg_param = create_rate_cg_param(),
  rate_ct_param = create_rate_ct_param(estimate = FALSE),
  rate_gt_param = create_rate_gt_param(),
  freq_equilibrium = "estimated"
) {
  beautier::create_site_model(
    name = "GTR",
    id = id,
    gamma_site_model = gamma_site_model,
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
    freq_equilibrium = freq_equilibrium
  )
}

#' Create an HKY site model
#' @inheritParams create_site_model
#' @param kappa the kappa
#' @param kappa_prior_distr the distribution of the kappa prior,
#'   which is a log-normal distribution
#'   (as created by \code{\link{create_log_normal_distr}})
#'   by default
#' @param freq_equilibrium the frequency in which the rates are at equilibrium
#'   are either \code{estimated}, \code{empirical} or \code{all_equal}.
#'   \code{\link{get_freq_equilibrium_names}} returns the possible values
#'   for \code{freq_equilibrium}
#' @return an HKY site_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'  hky_site_model <- create_hky_site_model()
#'
#'  create_beast2_input_file(
#'    input_filenames = get_fasta_filename(),
#'    "beast.xml",
#'    site_models = hky_site_model
#'  )
#' @aliases create_hky_site_model create_site_model_hky
#' @export create_hky_site_model create_site_model_hky
create_hky_site_model <- create_site_model_hky <- function(
  id = NA,
  kappa = "2.0",
  gamma_site_model = create_gamma_site_model(),
  kappa_prior_distr = create_log_normal_distr(
    m = create_m_param(value = "1.0"),
    s = create_s_param(value = "1.25")
  ),
  freq_equilibrium = "estimated"
) {
  beautier::create_site_model(
    name = "HKY",
    id = id,
    gamma_site_model = gamma_site_model,
    kappa = kappa,
    kappa_prior_distr = kappa_prior_distr,
    freq_equilibrium = freq_equilibrium
  )
}

#' Create a JC69 site model
#' @inheritParams create_site_model
#' @return a JC69 site_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'  jc69_site_model <- create_jc69_site_model()
#'
#'  create_beast2_input_file(
#'    input_filenames = get_fasta_filename(),
#'    "beast.xml",
#'    site_models = jc69_site_model
#'  )
#' @aliases create_jc69_site_model create_site_model_jc69
#' @export create_jc69_site_model create_site_model_jc69
create_jc69_site_model <- create_site_model_jc69 <- function(
  id = NA,
  gamma_site_model = create_gamma_site_model()
) {
  beautier::create_site_model(
    name = "JC69",
    id = id,
    gamma_site_model = gamma_site_model
  )
}

#' Create a TN93 site model
#' @inheritParams create_site_model
#' @param kappa_1_prior_distr the distribution of the kappa 1 prior,
#'   which is a log-normal distribution
#'   (as created by \code{\link{create_log_normal_distr}})
#'   by default
#' @param kappa_2_prior_distr the distribution of the kappa 2 prior,
#'   which is a log-normal distribution
#'   (as created by \code{\link{create_log_normal_distr}})
#'   by default
#' @param kappa_1_param the 'kappa 1' parameter,
#'   as returned by \code{\link{create_kappa_1_param}}
#' @param kappa_2_param the 'kappa 2' parameter,
#'   as returned by \code{\link{create_kappa_2_param}}
#' @param freq_equilibrium the frequency in which the rates are at equilibrium
#'   are either \code{estimated}, \code{empirical} or \code{all_equal}.
#'   \code{\link{get_freq_equilibrium_names}} returns the possible values
#'   for \code{freq_equilibrium}
#' @return a TN93 site_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'  tn93_site_model <- create_tn93_site_model()
#'
#'  create_beast2_input_file(
#'    input_filenames = get_fasta_filename(),
#'    "beast.xml",
#'    site_models = tn93_site_model
#'  )
#' @aliases create_tn93_site_model create_site_model_tn93
#' @export create_tn93_site_model create_site_model_tn93
create_tn93_site_model <- create_site_model_tn93 <- function(
  id = NA,
  gamma_site_model = create_gamma_site_model(),
  kappa_1_param = create_kappa_1_param(),
  kappa_2_param = create_kappa_2_param(),
  kappa_1_prior_distr = create_log_normal_distr(
    m = create_m_param(id = NA, estimate = FALSE, value = "1.0"),
    s = create_s_param(id = NA, estimate = FALSE, value = "1.25")
  ),
  kappa_2_prior_distr = create_log_normal_distr(
    m = create_m_param(id = NA, estimate = FALSE, value = "1.0"),
    s = create_s_param(id = NA, estimate = FALSE, value = "1.25")
  ),
  freq_equilibrium = "estimated"
) {
  beautier::create_site_model(
    name = "TN93",
    id = id,
    gamma_site_model = gamma_site_model,
    kappa_1_prior_distr = kappa_1_prior_distr,
    kappa_2_prior_distr = kappa_2_prior_distr,
    kappa_1_param = kappa_1_param,
    kappa_2_param = kappa_2_param,
    freq_equilibrium = freq_equilibrium
  )
}
