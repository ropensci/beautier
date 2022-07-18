#' Determines if the environment is AppVeyor
#' @return \link{TRUE} if run on AppVeyor, \link{FALSE} otherwise
#' @examples
#'   if (is_on_appveyor()) {
#'     message("Running on AppVeyor")
#'   }
#' @author Richèl J.C. Bilderbeek
#' @export
is_on_appveyor <- function() {
  Sys.getenv("APPVEYOR") != ""
}

#' Determines if the environment is GitHub Actions
#' @return \link{TRUE} if run on GitHub Actions, \link{FALSE} otherwise
#' @note It is possible to fake being on GitHub Actions, using:
#'
#' ```r
#' Sys.setenv(GITHUB_ACTIONS = "I fake being on GitHub Actions")
#' is_on_github_actions() # Will be true
#' ```
#'
#' To undo this, do
#'
#' ```r
#' Sys.setenv(GITHUB_ACTIONS = "")
#' is_on_github_actions() # Will be false
#' ```
#'
#' @examples
#' if (is_on_github_actions()) {
#'   message("Running on GitHub Actions")
#' }
#' @author Richèl J.C. Bilderbeek
#' @export
is_on_github_actions <- function() {
  Sys.getenv("GITHUB_ACTIONS") != ""
}

#' Determines if the environment is Travis CI
#' @return \link{TRUE} if run on Travis CI, \link{FALSE} otherwise
#' @examples
#'   if (is_on_ci()) {
#'     message("Running on Travis CI")
#'   }
#' @author Richèl J.C. Bilderbeek
#' @export
is_on_travis <- function() {
  Sys.getenv("TRAVIS") != ""
}

#' Determines if the environment is a continuous integration service
#' @return \link{TRUE} if run on AppVeyor or Travis CI, \link{FALSE} otherwise
#' @note It is possible to fake being on continuous integration service,
#' in this case GitHub Actions, using:
#'
#' ```r
#' Sys.setenv(GITHUB_ACTIONS = "I fake being on GitHub Actions")
#' is_on_ci() # Will be true
#' ```
#'
#' To undo this, do
#'
#' ```r
#' Sys.setenv(GITHUB_ACTIONS = "")
#' is_on_ci() # Will be false
#' ```
#'
#' @examples
#' is_on_ci()
#' @author Richèl J.C. Bilderbeek
#' @export
is_on_ci <- function() {
  beautier::is_on_appveyor() ||
    beautier::is_on_github_actions() ||
    beautier::is_on_travis()
}
