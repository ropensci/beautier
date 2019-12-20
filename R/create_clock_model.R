#' General function to create a clock model
#' @note Prefer using the named function
#'   \code{\link{create_rln_clock_model}}
#'   and \code{\link{create_strict_clock_model}}
#' @param name the clock model name. Valid
#'   names can be found in \code{get_clock_model_names}
#' @param id a clock model's ID
#' @param ... specific clock model parameters
#' @return a valid clock model
#' @seealso An alignment ID can be extracted from
#'   its FASTA filename using \code{\link{get_alignment_id}}.
#'   For more examples about creating a relaxed log-normal clock
#'   model, see \code{\link{create_rln_clock_model}}.
#'   For more examples about creating a strict clock
#'   model, see \code{\link{create_strict_clock_model}}.
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   rln_clock_model <- create_rln_clock_model()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     clock_model = rln_clock_model
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#'
#'   strict_clock_model <- create_strict_clock_model()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     clock_model = strict_clock_model
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @export
create_clock_model <- function(
  name,
  id,
  ...
) {
  if (!beautier::is_clock_model_name(name)) {
    clock_models_as_string <- function() {
      s <- NULL
      for (p in beautier::get_clock_model_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "invalid clock model name, must be one these: ",
      clock_models_as_string()
    )
  }
  clock_model <- list(name = name, id = id, ...)
  clock_model
}

#' Create a relaxed log-normal clock model
#' @inheritParams default_params_doc
#' @param mean_rate_prior_distr the mean clock rate prior distribution,
#'   as created by a \code{\link{create_distr}} function
#' @param ucldstdev_distr the standard deviation of the uncorrelated
#'   log-normal distribution,
#'   as created by a \code{\link{create_distr}} function
#' @param mparam_id the ID of the M parameter in the \code{branchRateModel},
#'   set to NA to have it initialized
#' @param mean_clock_rate the mean clock rate, 1.0 by default
#'   (is called \code{ucld_stdev} in XML, where \code{ucld_stdev} is always 0.1)
#' @param n_rate_categories the number of rate categories.
#'   -1 is default,
#'   0 denotes as much rates as branches
#' @param normalize_mean_clock_rate normalize the mean clock rate
#' @param dimension the dimensionality of the relaxed clock model.
#'   Leave NA to let beautier calculate it.
#'   Else, the dimensionality of the clock
#'   equals twice the number of taxa minus two.
#' @return a relaxed log-normal clock_model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   rln_clock_model <- create_rln_clock_model()
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     clock_model = rln_clock_model
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#'
#'   rln_clock_model_exp <- create_rln_clock_model(
#'     mean_rate_prior_distr = create_exp_distr()
#'   )
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     clock_model = rln_clock_model_exp
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_rln_clock_model create_clock_model_rln
#' @export create_rln_clock_model create_clock_model_rln
create_rln_clock_model <- create_clock_model_rln <- function(
  id = NA,
  mean_rate_prior_distr = create_uniform_distr(), # unknown default distr
  ucldstdev_distr = create_gamma_distr(),
  mparam_id = NA,
  mean_clock_rate = "1.0",
  n_rate_categories = -1,
  normalize_mean_clock_rate = FALSE,
  dimension = NA
) {
  rln_clock_model <- beautier::create_clock_model(
    name = "relaxed_log_normal",
    id = id,
    ucldstdev_distr = ucldstdev_distr,
    mean_rate_prior_distr = mean_rate_prior_distr,
    mparam_id = mparam_id,
    mean_clock_rate = mean_clock_rate,
    n_rate_categories = n_rate_categories,
    normalize_mean_clock_rate = normalize_mean_clock_rate,
    dimension = dimension
  )
  testit::assert(beautier::is_rln_clock_model(rln_clock_model))
  rln_clock_model
}

#' Create a strict clock model
#' @inheritParams default_params_doc
#' @param clock_rate_param the clock rate's parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by the \code{\link{create_clock_rate_param}} function
#' @param clock_rate_distr the clock rate's distribution,
#'   as created by a \code{\link{create_distr}} function
#' @return a strict clock_model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   strict_clock_model <- create_strict_clock_model(
#'     clock_rate_param = 1.0,
#'     clock_rate_distr = create_uniform_distr()
#'   )
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     clock_model = strict_clock_model
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#'
#'   strict_clock_model_gamma <- create_strict_clock_model(
#'     clock_rate_distr = create_gamma_distr()
#'   )
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     clock_model = strict_clock_model_gamma
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @aliases create_strict_clock_model create_clock_model_strict
#' @export create_strict_clock_model create_clock_model_strict
create_strict_clock_model <- create_clock_model_strict <- function(
  id = NA,
  clock_rate_param = create_clock_rate_param(),
  clock_rate_distr = create_uniform_distr()
) {
  if (beautier::is_one_double(clock_rate_param)) {
    clock_rate_param <- create_clock_rate_param(clock_rate_param)
  }
  if (!beautier::is_clock_rate_param(clock_rate_param)) {
    stop(
      "'clock_rate_param' must be a clock rate parameter, ",
      "as can be created by 'create_clock_rate_param'"
    )
  }
  if (!beautier::is_distr(clock_rate_distr)) {
    stop(
      "'clock_rate_distr' must be a distribution, ",
      "as can be created by 'create_distr'"
    )
  }
  strict_clock_model <- beautier::create_clock_model(
    name = "strict",
    id = id,
    clock_rate_param = clock_rate_param,
    clock_rate_distr = clock_rate_distr
  )
  testit::assert(beautier::is_strict_clock_model(strict_clock_model))
  strict_clock_model
}
