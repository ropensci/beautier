#' Check if the misc options is a valid misc options object.
#'
#' Calls \code{stop} if the misc options object is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_beauti_options} to create a valid MCMC
#' @examples
#'  testthat::expect_silent(check_beauti_options(create_beauti_options()))
#'
#'  # Must stop on non-MCMCs
#'  testthat::expect_error(check_beauti_options(beauti_options = "nonsense"))
#'  testthat::expect_error(check_beauti_options(beauti_options = NULL))
#'  testthat::expect_error(check_beauti_options(beauti_options = NA))
#' @author Richel J.C. Bilderbeek
#' @export
check_beauti_options <- function(
  beauti_options
) {
  argument_names <- c(
    "capitalize_first_char_id",
    "nucleotides_uppercase",
    "beast2_version",
    "required",
    "sequence_indent"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(beauti_options)) {
      stop(
        "'", arg_name, "' must be an element of an 'beauti_options'. ",
        "Tip: use 'create_beauti_options'"
      )
    }
  }
  if (length(beauti_options$capitalize_first_char_id) != 1 ||
    is.na(beauti_options$capitalize_first_char_id) ||
    !is.logical(beauti_options$capitalize_first_char_id)) {
    stop("'capitalize_first_char_id' must be one boolean")
  }
  if (length(beauti_options$nucleotides_uppercase) != 1 ||
    is.na(beauti_options$nucleotides_uppercase) ||
    !is.logical(beauti_options$nucleotides_uppercase)) {
    stop("'nucleotides_uppercase' must be one boolean")
  }
  if (length(beauti_options$beast2_version) != 1 ||
    !is.character(beauti_options$beast2_version)) {
    stop("'beast2_version' must be one character string")
  }
  if (length(beauti_options$required) != 1 ||
    !is.character(beauti_options$required)) {
    stop("'required' must be one character string")
  }
  if (length(beauti_options$sequence_indent) != 1 ||
    !is.numeric(beauti_options$sequence_indent)) {
    stop("'sequence_indent' must be one number")
  }
}
