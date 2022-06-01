#' Creates the \code{freq_equilibrium} as XML
#' @param freq_equilibrium a \code{freq_equilibrium} name
#' @param id a site model's name
#' @return the \code{freq_equilibrium} as XML
#' @examples
#' check_empty_beautier_folder()
#'
#' freq_equilibrium_to_xml(freq_equilibrium = "estimated", id = "my_id")
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
freq_equilibrium_to_xml <- function(
  freq_equilibrium,
  id
) {
  testit::assert(beautier::is_freq_equilibrium_name(freq_equilibrium))
  if (freq_equilibrium == "estimated") {
    paste0("<frequencies ", "id=\"estimatedFreqs.s:", id, "\" ",
      "spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>" # nolint this is no absolute path
    )
  } else if (freq_equilibrium == "empirical") {
    paste0("<frequencies id=\"empiricalFreqs.s:", id, "\" ",
      "spec=\"Frequencies\" data=\"@", id, "\"/>" # nolint this is no absolute path
    )
  } else {
    testit::assert(freq_equilibrium == "all_equal")
    paste0("<frequencies id=\"equalFreqs.s:", id, "\" ",
      "spec=\"Frequencies\" data=\"@", id, "\" estimate=\"false\"/>") # nolint this is no absolute path
  }
}
