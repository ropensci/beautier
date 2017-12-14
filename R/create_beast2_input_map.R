#' Creates the map section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_map <- function() {
  text <- NULL
  text <- c(text,
    "<map name=\"Uniform\" >beast.math.distributions.Uniform</map>")
  text <- c(text,
    "<map name=\"Exponential\" >beast.math.distributions.Exponential</map>")
  text <- c(text, paste0("<map name=\"LogNormal\" >",
    "beast.math.distributions.LogNormalDistributionModel</map>"))
  text <- c(text, "<map name=\"Normal\" >beast.math.distributions.Normal</map>")
  text <- c(text, "<map name=\"Beta\" >beast.math.distributions.Beta</map>")
  text <- c(text, "<map name=\"Gamma\" >beast.math.distributions.Gamma</map>")
  text <- c(text, paste0("<map name=\"LaplaceDistribution\" >",
    "beast.math.distributions.LaplaceDistribution</map>"))
  text <- c(text, "<map name=\"prior\" >beast.math.distributions.Prior</map>")
  text <- c(text, paste0("<map name=\"InverseGamma\" >",
    "beast.math.distributions.InverseGamma</map>"))
  text <- c(text, "<map name=\"OneOnX\" >beast.math.distributions.OneOnX</map>")
  text
}
