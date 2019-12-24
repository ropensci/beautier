#' Determine if the object is a valid \code{beauti_options}
#' @param x an object, to be determined if it is a \code{beauti_options}
#' @return \link{TRUE} if the object is a valid \code{beauti_options},
#'   \link{FALSE} otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_true(is_beauti_options(create_beauti_options()))
#'
#' expect_false(is_beauti_options("nonsense"))
#' expect_false(is_beauti_options(NA))
#' expect_false(is_beauti_options(NULL))
#' expect_false(is_beauti_options(""))
#' expect_false(is_beauti_options(c()))
#' @seealso use \link{create_beauti_options} to create a valid
#' \code{beauti_options} object
#' @export
is_beauti_options <- function(
  x
) {
  if (!"capitalize_first_char_id" %in% names(x)) return(FALSE)
  if (!"nucleotides_uppercase" %in% names(x)) return(FALSE)
  if (!"sequence_indent" %in% names(x)) return(FALSE)
  TRUE
}
