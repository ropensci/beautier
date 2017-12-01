#' Creates the \code{freq_equilibrium} as XML
#' @param freq_equilibrium a \code{freq_equilibrium} name
#' @param id a site model's name
#' @return the \code{freq_equilibrium} as XML
freq_equilibrium_to_xml <- function(
  freq_equilibrium,
  id
) {
  testit::assert(is_freq_equilibrium_name(freq_equilibrium)) # nolint internal function
  if (freq_equilibrium == "estimated") {
    paste0("<frequencies ", "id=\"estimatedFreqs.s:", id, "\" ",
      "spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>"
    )
  } else if (freq_equilibrium == "empirical") {
    paste0("<frequencies id=\"empiricalFreqs.s:", id, "\" ",
      "spec=\"Frequencies\" data=\"@", id, "\"/>"
    )
  } else {
    testit::assert(freq_equilibrium == "all_equal")
    paste0("<frequencies id=\"equalFreqs.s:", id, "\" ",
      "spec=\"Frequencies\" data=\"@", id, "\" estimate=\"false\"/>")
  }
}
