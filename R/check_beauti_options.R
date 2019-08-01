#' Check if the \code{beauti_options} is a valid \code{beauti_options} object.
#'
#' Calls \code{stop} if the \code{beauti_options} object is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_beauti_options} to create a valid
#'   BEAUti options setup
#' @examples
#'  testthat::expect_silent(check_beauti_options(create_beauti_options()))
#'
#'  # Must stop on nonsense
#'  testthat::expect_error(check_beauti_options(beauti_options = "nonsense"))
#'  testthat::expect_error(check_beauti_options(beauti_options = NULL))
#'  testthat::expect_error(check_beauti_options(beauti_options = NA))
#' @author Richèl J.C. Bilderbeek
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
  if (!is_one_bool(beauti_options$capitalize_first_char_id)) { # nolint beautier function
    stop(
      "'capitalize_first_char_id' must be one boolean. \n",
      "Actual value: ", beauti_options$capitalize_first_char_id
    )
  }
  if (!is_one_bool(beauti_options$nucleotides_uppercase)) { # nolint beautier function
    stop(
      "'nucleotides_uppercase' must be one boolean. \n",
      "Actual value: ", beauti_options$nucleotides_uppercase
    )
  }
  assertive::assert_is_a_string(beauti_options$beast2_version)
  assertive::assert_is_a_string(beauti_options$required)
  if (!is_one_int(beauti_options$sequence_indent)) { #  nolint beautier function
    stop(
      "'sequence_indent' must be one number. \n",
      "Actual value: ", beauti_options$sequence_indent
    )
  }
}
