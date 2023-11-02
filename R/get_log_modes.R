#' Get the possible log modes
#' @return the possible log modes
#' @examples
#' get_log_modes()
#' @author Richèl J.C. Bilderbeek
#' @export
get_log_modes <- function() {
  c(
    "autodetect",
    "compound",
    "tree"
  )
}
