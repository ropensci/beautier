#' Create a gamma site model, part of a site model
#' @param gamma_cat_count the number of gamma categories, must
#'   be an integer with value zero or more
#' @param gamma_shape gamma curve shape parameter
#' @param prop_invariant the proportion invariant, must be a value
#'   from 0.0 to 1.0
#' @param gamma_shape_prior_distr the distribution of the gamma shape prior.
#'   \code{gamma_shape_prior_distr} must be \code{NA} for
#'   a \code{gamma_cat_count} of zero or one.
#'   For a \code{gamma_cat_count} of two or more,
#'   leaving \code{gamma_shape_prior_distr} equal to its default
#'   value of \code{NA}, a default distribution is used.
#'   Else \code{gamma_shape_prior_distr} must be a
#'   distribution, as can be created by \code{\link{create_distr}}
#' @param freq_equilibrium the frequency in which the rates are at equilibrium
#'   are either \code{estimated}, \code{empirical} or \code{all_equal}.
#'   \code{get_freq_equilibrium_names} returns the possible values
#'   for \code{freq_equilibrium}
#' @return a gamma site model
#' @note Use \code{\link{create_site_model}} to create a site model
#'   that has both a gamma site model and substitution model
#' @examples
#'   gamma_site_model <- create_gamma_site_model(prop_invariant = 0.5)
#'
#'   site_model <- create_hky_site_model(gamma_site_model = gamma_site_model)
#'
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     "create_gamma_site_model.xml",
#'     site_model = site_model
#'   )
#'   testit::assert(file.exists("create_gamma_site_model.xml"))
#' @author Richel J.C. Bilderbeek
#' @export
create_gamma_site_model <- function(
  gamma_cat_count = "0",
  gamma_shape = "1.0",
  prop_invariant = "0.0",
  gamma_shape_prior_distr = NA,
  freq_equilibrium = "estimated"
) {
  if (gamma_cat_count < 0) {
    stop("'gamma_cat_count' must be positive")
  }
  if (gamma_shape < 0.0) {
    stop("'gamma_shape' must be positive")
  }
  if (prop_invariant < 0.0 || prop_invariant > 1.0) {
    stop("'prop_invariant' must be in range [0.0, 1.0]")
  }
  if (gamma_cat_count >= 2 && is.na(gamma_shape_prior_distr)) {
    # Cannot simplify, due to 1.0 becomes 1 in XML
    gamma_shape_prior_distr <- create_exp_distr(
        id = NA,
        mean = create_mean_param(
          id = NA,
          value = "1.0" # string to match XML
        )
      )
  }
  if (!is.na(gamma_shape_prior_distr) && !is_distr(gamma_shape_prior_distr)) {
    stop("'gamma_shape_prior_distr' must be a distribution")
  }
  if (gamma_cat_count < 2 && !is.na(gamma_shape_prior_distr)) {
    stop(
      "'gamma_shape_prior_distr' must be NA ",
      "for a 'gamma_cat_count' of less than two"
    )
  }

  gamma_site_model <- list(
    gamma_cat_count = gamma_cat_count,
    gamma_shape = gamma_shape,
    prop_invariant = prop_invariant,
    gamma_shape_prior_distr = gamma_shape_prior_distr
  )
  testit::assert(is_gamma_site_model(gamma_site_model)) # nolint internal function
  gamma_site_model
}
