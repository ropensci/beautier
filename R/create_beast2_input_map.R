#' Creates the map section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_map <- function(
  beauti_options
) {
  text <- c(
    "<map name=\"Uniform\" >beast.math.distributions.Uniform</map>",
    "<map name=\"Exponential\" >beast.math.distributions.Exponential</map>",
    paste0(
      "<map name=\"LogNormal\" >",
      "beast.math.distributions.LogNormalDistributionModel</map>"
    ),
    "<map name=\"Normal\" >beast.math.distributions.Normal</map>",
    "<map name=\"Beta\" >beast.math.distributions.Beta</map>",
    "<map name=\"Gamma\" >beast.math.distributions.Gamma</map>",
    paste0(
      "<map name=\"LaplaceDistribution\" >",
      "beast.math.distributions.LaplaceDistribution</map>"
    ),
    "<map name=\"prior\" >beast.math.distributions.Prior</map>",
    paste0(
      "<map name=\"InverseGamma\" >",
      "beast.math.distributions.InverseGamma</map>"
    ),
    "<map name=\"OneOnX\" >beast.math.distributions.OneOnX</map>"
  )
  if (beauti_options$beast2_version == "2.6") {
    text <- interspace(text)
    text <- c(text, "")
  }
  text
}
