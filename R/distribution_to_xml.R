#' Converts a distribution to XML
#' @param distribution a distibution,
#'   as created by \code{\link{create_distribution}})
#' @param n_spaces the number of spaces to add before the text
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
#' @export
distribution_to_xml <- function(
  distribution,
  n_spaces
) {
  text <- NULL
  id <- beautier::get_distribution_id(distribution)
  if (is.na(id)) {
    stop("distribution must have an ID")
  }
  if (is_uniform_distribution(distribution)) {
    text <- c(text, paste0("<Uniform ",
      "id=\"Uniform.", id, "\" ",
      "name=\"distr\" upper=\"Infinity\"/>"))
  } else if (is_normal_distribution(distribution)) {
    text <- c(text, paste0("<Normal ",
      "id=\"Normal.", id, "\" name=\"distr\">"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.1\" estimate=\"false\" ",
      "name=\"mean\">0.0</parameter>"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.2\" estimate=\"false\" ",
      "name=\"sigma\">1.0</parameter>"))
    text <- c(text, paste0("</Normal>"))
  } else if (is_one_div_x_distribution(distribution)) {
    text <- c(text, paste0("<OneOnX ",
      "id=\"OneOnX.", id, "\" name=\"distr\"/>"))
  } else if (is_log_normal_distribution(distribution)) {
    text <- c(text, paste0("<LogNormal ",
      "id=\"LogNormalDistributionModel.", id, "\" name=\"distr\">"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.3\" estimate=\"false\" name=\"M\">1.0</parameter>"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.4\" estimate=\"false\" lower=\"0.0\" ",
      "name=\"S\" upper=\"5.0\">1.25</parameter>"))
    text <- c(text, paste0("</LogNormal>"))
  } else if (is_exponential_distribution(distribution)) {
    text <- c(text, paste0("<Exponential ",
      "id=\"Exponential.", id, "\" name=\"distr\">"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.5\" estimate=\"false\" ",
      "name=\"mean\">1.0</parameter>"))
    text <- c(text, paste0("</Exponential>"))
  } else if (is_gamma_distribution(distribution)) {
    text <- c(text, paste0("<Gamma ",
      "id=\"Gamma.", id, "\" name=\"distr\">"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.6\" estimate=\"false\" ",
      "name=\"alpha\">2.0</parameter>"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.7\" estimate=\"false\" ",
      "name=\"beta\">2.0</parameter>"))
    text <- c(text, paste0("</Gamma>"))
  } else if (is_beta_distribution(distribution)) {
    text <- c(text, paste0("<Beta ",
      "id=\"Beta.", id, "\" name=\"distr\">"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.8\" estimate=\"false\" ",
      "name=\"alpha\">2.0</parameter>"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.9\" estimate=\"false\" ",
      "name=\"beta\">2.0</parameter>"))
    text <- c(text, paste0("</Beta>"))
  } else if (is_laplace_distribution(distribution)) {
    text <- c(text, paste0("<LaplaceDistribution ",
      "id=\"LaplaceDistribution.", id, "\" name=\"distr\">"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.10\" estimate=\"false\" ",
      "name=\"mu\">0.0</parameter>"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.11\" estimate=\"false\" ",
      "name=\"scale\">1.0</parameter>"))
    text <- c(text, paste0("</LaplaceDistribution>"))
  } else if (is_inv_gamma_distribution(distribution)) {
    text <- c(text, paste0("<InverseGamma ",
      "id=\"InverseGamma.", id, "\" name=\"distr\">"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.12\" estimate=\"false\" ",
      "name=\"alpha\">2.0</parameter>"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.13\" estimate=\"false\" ",
      "name=\"beta\">2.0</parameter>"))
    text <- c(text, paste0("</InverseGamma>"))
  } else if (is_poisson_distribution(distribution)) {
    text <- c(text, paste0("<distr ",
      "id=\"Poisson.", id, "\" ",
      "spec=\"beast.math.distributions.Poisson\">"))
    text <- c(text, paste0("    <parameter ",
      "id=\"RealParameter.14\" name=\"lambda\">0.693</parameter>"))
    text <- c(text, paste0("</distr>"))
  }
  beautier::indent(text = text, n_spaces = n_spaces)
}
