#' General function to create a clock model
#' @note Prefer using the named function
#'   \code{\link{create_rln_clock_model}}
#'   and \code{\link{create_strict_clock_model}}
#' @param name the clock model name. Valid
#'   names can be found in \code{\link{get_clock_model_names}}
#' @param ... specific clock model parameters
#' @return a clock_model
#' @author Richel J.C. Bilderbeek
#' @export
create_clock_model <- function(
  name,
  ...
) {
  if (!is_clock_model_name(name)) {
    clock_models_as_string <- function() {
      s <- NULL
      for (p in get_clock_model_names()) {
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
  clock_model <- list(name = name, ...)
  clock_model
}

#' Create a relaxed log-normal clock model
#' @param uclstdev_distr the uclstdev distribution,
#'   as created by a \code{\link{create_distr}} function
#' @param m_parameter_id the ID of the M paramater in the branchRateModel,
#'   set to NA to have it initialized
#' @return a relaxed log-normal clock_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rln_clock_model <- create_rln_clock_model()
#'   testit::assert(is_rln_clock_model(rln_clock_model))
#'
#'   input_fasta_filenames <- system.file(
#'     "extdata", "test_output_0.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filenames,
#'     "my_beast.xml",
#'     clock_models = rln_clock_model
#'   )
#' @export
create_rln_clock_model <- function(
  uclstdev_distr = create_gamma_distr(),
  m_parameter_id = NA
) {
  rln_clock_model <- beautier::create_clock_model(
    name = "relaxed_log_normal",
    uclstdev_distr = uclstdev_distr,
    m_parameter_id = m_parameter_id
  )
  testit::assert(beautier::is_rln_clock_model(rln_clock_model))
  rln_clock_model
}

#' Create a strict clock model
#' @param clock_rate_parameter a clock_rate parameter,
#'   as created by a \code{\link{create_clock_rate_parameter}} function
#' @return a strict clock_model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   strict_clock_model <- create_strict_clock_model()
#'   testit::assert(is_strict_clock_model(strict_clock_model))
#'
#'   input_fasta_filenames <- system.file(
#'     "extdata", "test_output_0.fas", package = "beautier"
#'   )
#'   create_beast2_input_file(
#'     input_fasta_filenames = input_fasta_filenames,
#'     "my_beast.xml",
#'     clock_models = strict_clock_model
#'   )
#' @export
create_strict_clock_model <- function(
  clock_rate_parameter = create_clock_rate_parameter()
) {
  strict_clock_model <- beautier::create_clock_model(
    name = "strict",
    clock_rate_parameter = clock_rate_parameter
  )
  testit::assert(beautier::is_strict_clock_model(strict_clock_model))
  strict_clock_model
}
