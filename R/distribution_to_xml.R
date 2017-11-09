#' Converts a distribution to XML
#' @param distribution a distibution,
#'   as created by \code{\link{create_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
#' @export
distribution_to_xml <- function(
  distribution
) {
  text <- NULL
  id <- beautier::get_distribution_id(distribution)
  if (is.na(id)) {
    stop("distribution must have an ID")
  }
  if (is_uniform_distribution(distribution)) {
    text <- c(text, distribution_to_xml_uniform(distribution))
  } else if (is_normal_distribution(distribution)) {
    text <- c(text, distribution_to_xml_normal(distribution))
  } else if (is_one_div_x_distribution(distribution)) {
    text <- c(text, distribution_to_xml_one_div_x(distribution))
  } else if (is_log_normal_distribution(distribution)) {
    text <- c(text, distribution_to_xml_log_normal(distribution))
  } else if (is_exponential_distribution(distribution)) {
    text <- c(text, distribution_to_xml_exponential(distribution))
  } else if (is_gamma_distribution(distribution)) {
    text <- c(text, distribution_to_xml_gamma(distribution))
  } else if (is_beta_distribution(distribution)) {
    text <- c(text, distribution_to_xml_beta(distribution))
  } else if (is_laplace_distribution(distribution)) {
    text <- c(text, distribution_to_xml_laplace(distribution))
  } else if (is_inv_gamma_distribution(distribution)) {
    text <- c(text, distribution_to_xml_inv_gamma(distribution))
  } else if (is_poisson_distribution(distribution)) {
    text <- c(text, distribution_to_xml_poisson(distribution))
  }
  text
}

#' Converts a beta distribution to XML
#' @param distribution a beta distibution,
#'   as created by \code{\link{create_beta_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distribution_to_xml_beta <- function(
  distribution
) {
  testit::assert(is_beta_distribution(distribution))
  id <- beautier::get_distribution_id(distribution)
  testit::assert(!is.na(id))

  text <- NULL
  text <- c(text, paste0("<Beta ",
    "id=\"Beta.", id, "\" name=\"distr\">"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.8\" estimate=\"false\" ",
    "name=\"alpha\">2.0</parameter>"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.9\" estimate=\"false\" ",
    "name=\"beta\">2.0</parameter>"))
  text <- c(text, paste0("</Beta>"))
  text
}

#' Converts an exponential distribution to XML
#' @param distribution an exponential distibution,
#'   as created by \code{\link{create_exponential_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distribution_to_xml_exponential <- function(
  distribution
) {
  testit::assert(is_exponential_distribution(distribution))
  id <- beautier::get_distribution_id(distribution)
  testit::assert(!is.na(id))

  text <- NULL
  text <- c(text, paste0("<Exponential ",
    "id=\"Exponential.", id, "\" name=\"distr\">"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.5\" estimate=\"false\" ",
    "name=\"mean\">1.0</parameter>"))
  text <- c(text, paste0("</Exponential>"))
  text
}

#' Converts a gamma distribution to XML
#' @param distribution a gamma distibution,
#'   as created by \code{\link{create_gamma_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distribution_to_xml_gamma <- function(
  distribution
) {
  testit::assert(is_gamma_distribution(distribution))
  id <- beautier::get_distribution_id(distribution)
  testit::assert(!is.na(id))

  text <- NULL
  text <- c(text, paste0("<Gamma ",
    "id=\"Gamma.", id, "\" name=\"distr\">"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.6\" estimate=\"false\" ",
    "name=\"alpha\">2.0</parameter>"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.7\" estimate=\"false\" ",
    "name=\"beta\">2.0</parameter>"))
  text <- c(text, paste0("</Gamma>"))
  text
}

#' Converts a inv_gamma distribution to XML
#' @param distribution a inv_gamma distibution,
#'   as created by \code{\link{create_inv_gamma_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distribution_to_xml_inv_gamma <- function(
  distribution
) {
  testit::assert(is_inv_gamma_distribution(distribution))
  id <- beautier::get_distribution_id(distribution)
  testit::assert(!is.na(id))

  text <- NULL
  text <- c(text, paste0("<InverseGamma ",
    "id=\"InverseGamma.", id, "\" name=\"distr\">"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.12\" estimate=\"false\" ",
    "name=\"alpha\">2.0</parameter>"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.13\" estimate=\"false\" ",
    "name=\"beta\">2.0</parameter>"))
  text <- c(text, paste0("</InverseGamma>"))
  text
}

#' Converts a laplace distribution to XML
#' @param distribution a laplace distibution,
#'   as created by \code{\link{create_laplace_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distribution_to_xml_laplace <- function(
  distribution
) {
  testit::assert(is_laplace_distribution(distribution))
  id <- beautier::get_distribution_id(distribution)
  testit::assert(!is.na(id))

  text <- NULL
  text <- c(text, paste0("<LaplaceDistribution ",
    "id=\"LaplaceDistribution.", id, "\" name=\"distr\">"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.10\" estimate=\"false\" ",
    "name=\"mu\">0.0</parameter>"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.11\" estimate=\"false\" ",
    "name=\"scale\">1.0</parameter>"))
  text <- c(text, paste0("</LaplaceDistribution>"))
  text
}
#' Converts a log-normal distribution to XML
#' @param distribution a log-normal distibution,
#'   as created by \code{\link{create_log_normal_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distribution_to_xml_log_normal <- function(
  distribution
) {
  testit::assert(is_log_normal_distribution(distribution))
  id <- beautier::get_distribution_id(distribution)
  testit::assert(!is.na(id))

  text <- NULL
  text <- c(text, paste0("<LogNormal ",
    "id=\"LogNormalDistributionModel.", id, "\" name=\"distr\">"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.3\" estimate=\"false\" name=\"M\">1.0</parameter>"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.4\" estimate=\"false\" lower=\"0.0\" ",
    "name=\"S\" upper=\"5.0\">1.25</parameter>"))
  text <- c(text, paste0("</LogNormal>"))
  text
}


#' Converts a normal distribution to XML
#' @param distribution a normal distibution,
#'   as created by \code{\link{create_normal_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distribution_to_xml_normal <- function(
  distribution
) {
  testit::assert(is_normal_distribution(distribution))
  id <- beautier::get_distribution_id(distribution)
  testit::assert(!is.na(id))

  text <- NULL
  text <- c(text, paste0("<Normal ",
    "id=\"Normal.", id, "\" name=\"distr\">"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.1\" estimate=\"false\" ",
    "name=\"mean\">0.0</parameter>"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.2\" estimate=\"false\" ",
    "name=\"sigma\">1.0</parameter>"))
  text <- c(text, paste0("</Normal>"))
  text
}

#' Converts a 1/x distribution to XML
#' @param distribution a 1/x distibution,
#'   as created by \code{\link{create_one_div_x_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distribution_to_xml_one_div_x <- function(
  distribution
) {
  testit::assert(is_one_div_x_distribution(distribution))
  id <- beautier::get_distribution_id(distribution)
  testit::assert(!is.na(id))

  text <- NULL
  text <- c(text, paste0("<OneOnX ",
    "id=\"OneOnX.", id, "\" name=\"distr\"/>"))
  text
}

#' Converts a poisson distribution to XML
#' @param distribution a poisson distibution,
#'   as created by \code{\link{create_poisson_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distribution_to_xml_poisson <- function(
  distribution
) {
  testit::assert(is_poisson_distribution(distribution))
  id <- beautier::get_distribution_id(distribution)
  testit::assert(!is.na(id))

  text <- NULL
  text <- c(text, paste0("<distr ",
    "id=\"Poisson.", id, "\" ",
    "spec=\"beast.math.distributions.Poisson\">"))
  text <- c(text, paste0("    <parameter ",
    "id=\"RealParameter.14\" name=\"lambda\">0.693</parameter>"))
  text <- c(text, paste0("</distr>"))
  text
}

#' Converts a uniform distribution to XML
#' @param distribution a uniform distibution,
#'   as created by \code{\link{create_uniform_distribution}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distribution_to_xml_uniform <- function(
  distribution
) {
  testit::assert(is_uniform_distribution(distribution))
  id <- beautier::get_distribution_id(distribution)
  testit::assert(!is.na(id))

  text <- NULL
  line_begin <- paste0("<Uniform id=\"Uniform.", id, "\" name=\"distr\"")
  line_end <- "/>"
  upper <- distribution$upper
  if (is.na(upper)) {
    text <- c(text, paste0(line_begin, line_end))
  } else if (is.infinite(upper)) {
    text <- c(text, paste0(line_begin, " upper=\"Infinity\"", line_end))
  } else {
    text <- c(text, paste0(line_begin, " upper=\"", upper, "\"", line_end))
  }
  text
}
