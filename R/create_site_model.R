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
#' @return a site_model
#' @author Richel J.C. Bilderbeek
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

#' Create a JC69 site model
#' @inheritParams create_site_model
#' @return a JC69 site_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'  jc69_site_model <- create_jc69_site_model()
#'  testit::assert(is_jc69_site_model(jc69_site_model))
#'
#'  create_beast2_input_file(
#'    input_fasta_filenames = get_input_fasta_filename(),
#'    "beast.xml",
#'    site_models = jc69_site_model
#'  )
#' @export
create_jc69_site_model <- function(
  id = NA,
  gamma_site_model = create_gamma_site_model()
) {
  beautier::create_site_model(
    name = "JC69",
    id = id,
    gamma_site_model = gamma_site_model
  )
}

#' Create an HKY site model
#' @inheritParams create_site_model
#' @param kappa the kappa
#' @param kappa_prior_distr the distribution of the kappa prior,
#'   which is a log-normal distribution
#'   (as created by \code{\link{create_log_normal_distr}})
#'   by default
#' @return an HKY site_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'  hky_site_model <- create_hky_site_model()
#'  testit::assert(is_hky_site_model(hky_site_model))
#'
#'  create_beast2_input_file(
#'    input_fasta_filenames = get_input_fasta_filename(),
#'    "beast.xml",
#'    site_models = hky_site_model
#'  )
#' @export
create_hky_site_model <- function(
  id = NA,
  kappa = get_default_kappa(),
  gamma_site_model = create_gamma_site_model(),
  kappa_prior_distr = create_log_normal_distr(
    m = create_m_param(value = "1.0"),
    s = create_s_param(value = "1.25")
  )
) {
  beautier::create_site_model(
    name = "HKY",
    id = id,
    gamma_site_model = gamma_site_model,
    kappa = kappa,
    kappa_prior_distr = kappa_prior_distr
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
#' @return a TN93 site_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'  tn93_site_model <- create_tn93_site_model()
#'  testit::assert(is_tn93_site_model(tn93_site_model))
#'
#'  create_beast2_input_file(
#'    input_fasta_filenames = get_input_fasta_filename(),
#'    "beast.xml",
#'    site_models = tn93_site_model
#'  )
#' @export
create_tn93_site_model <- function(
  id = NA,
  gamma_site_model = create_gamma_site_model(),
  kappa_1_prior_distr = create_log_normal_distr(
    m = create_m_param(id = NA, estimate = FALSE, value = "1.0"),
    s = create_s_param(id = NA, estimate = FALSE, value = "1.25")
  ),
  kappa_2_prior_distr = create_log_normal_distr(
    m = create_m_param(id = NA, estimate = FALSE, value = "1.0"),
    s = create_s_param(id = NA, estimate = FALSE, value = "1.25")
  )
) {
  beautier::create_site_model(
    name = "TN93",
    id = id,
    gamma_site_model = gamma_site_model,
    kappa_1_prior_distr = kappa_1_prior_distr,
    kappa_2_prior_distr = kappa_2_prior_distr
  )
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
#' @return a GTR site_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'  gtr_site_model <- create_gtr_site_model()
#'  testit::assert(is_gtr_site_model(gtr_site_model))
#'
#'  create_beast2_input_file(
#'    input_fasta_filenames = get_input_fasta_filename(),
#'    "beast.xml",
#'    site_models = gtr_site_model
#'  )
#' @export
create_gtr_site_model <- function(
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
  )
) {
  beautier::create_site_model(
    name = "GTR",
    id = id,
    gamma_site_model = gamma_site_model,
    rate_ac_prior_distr = rate_ac_prior_distr,
    rate_ag_prior_distr = rate_ag_prior_distr,
    rate_at_prior_distr = rate_at_prior_distr,
    rate_cg_prior_distr = rate_cg_prior_distr,
    rate_gt_prior_distr = rate_gt_prior_distr
  )
}
