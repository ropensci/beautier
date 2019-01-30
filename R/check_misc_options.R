#' Check if the misc options is a valid misc options object.
#'
#' Calls \code{stop} if the misc options object is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_misc_options} to create a valid MCMC
#' @examples
#'  testthat::expect_silent(check_misc_options(create_misc_options()))
#'
#'  # Must stop on non-MCMCs
#'  testthat::expect_error(check_misc_options(misc_options = "nonsense"))
#'  testthat::expect_error(check_misc_options(misc_options = NULL))
#'  testthat::expect_error(check_misc_options(misc_options = NA))
#' @author Richel J.C. Bilderbeek
#' @export
check_misc_options <- function(
  misc_options
) {
  argument_names <- c(
    "capitalize_first_char_id",
    "nucleotides_uppercase",
    "beast2_version",
    "required",
    "sequence_indent"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(misc_options)) {
      stop(
        "'", arg_name, "' must be an element of an 'misc_options'. ",
        "Tip: use 'create_misc_options'"
      )
    }
  }
  if (length(misc_options$capitalize_first_char_id) != 1 ||
    is.na(misc_options$capitalize_first_char_id) ||
    !is.logical(misc_options$capitalize_first_char_id)) {
    stop("'capitalize_first_char_id' must be one boolean")
  }
  if (length(misc_options$nucleotides_uppercase) != 1 ||
    is.na(misc_options$nucleotides_uppercase) ||
    !is.logical(misc_options$nucleotides_uppercase)) {
    stop("'nucleotides_uppercase' must be one boolean")
  }
  if (length(misc_options$beast2_version) != 1 ||
    !is.character(misc_options$beast2_version)) {
    stop("'beast2_version' must be one character string")
  }
  if (length(misc_options$required) != 1 ||
    !is.character(misc_options$required)) {
    stop("'required' must be one character string")
  }
  if (length(misc_options$sequence_indent) != 1 ||
    !is.numeric(misc_options$sequence_indent)) {
    stop("'sequence_indent' must be one number")
  }
}
