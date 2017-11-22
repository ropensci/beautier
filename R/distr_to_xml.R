#' Converts a distribution to XML
#' @param distr a distibution,
#'   as created by \code{\link{create_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
#' @export
distr_to_xml <- function(
  distr
) {
  text <- NULL
  id <- beautier::get_distr_id(distr)
  if (!beautier::is_id(id)) {
    stop("distribution must have an ID")
  }
  if (is_beta_distr(distr)) {
    text <- c(text, distr_to_xml_beta(distr)) # nolint internal function
  } else if (is_exponential_distr(distr)) {
    text <- c(text, distr_to_xml_exponential(distr)) # nolint internal function
  } else if (is_gamma_distr(distr)) {
    text <- c(text, distr_to_xml_gamma(distr)) # nolint internal function
  } else if (is_inv_gamma_distr(distr)) {
    text <- c(text, distr_to_xml_inv_gamma(distr)) # nolint internal function
  } else if (is_laplace_distr(distr)) {
    text <- c(text, distr_to_xml_laplace(distr)) # nolint internal function
  } else if (is_log_normal_distr(distr)) {
    text <- c(text, distr_to_xml_log_normal(distr)) # nolint internal function
  } else if (is_normal_distr(distr)) {
    text <- c(text, distr_to_xml_normal(distr)) # nolint internal function
  } else if (is_one_div_x_distr(distr)) {
    text <- c(text, distr_to_xml_one_div_x(distr)) # nolint internal function
  } else if (is_poisson_distr(distr)) {
    text <- c(text, distr_to_xml_poisson(distr)) # nolint internal function
  } else {
    testit::assert(beautier::is_uniform_distr(distr))
    text <- c(text, distr_to_xml_uniform(distr)) # nolint internal function
  }
  text
}

#' Converts a beta distribution to XML
#' @param distr a beta distibution,
#'   as created by \code{\link{create_beta_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distr_to_xml_beta <- function(
  distr
) {
  testit::assert(beautier::is_beta_distr(distr))
  id <- beautier::get_distr_id(distr)
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<Beta id=\"Beta.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent(
      parameter_to_xml(distr$alpha),
      n_spaces = 4
    )
  )
  text <- c(text,
    indent(
      parameter_to_xml(distr$beta),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</Beta>"))
  text
}

#' Converts an exponential distribution to XML
#' @param distr an exponential distibution,
#'   as created by \code{\link{create_exponential_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distr_to_xml_exponential <- function(
  distr
) {
  testit::assert(beautier::is_exponential_distr(distr))
  id <- beautier::get_distr_id(distr)
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<Exponential ",
    "id=\"Exponential.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent(
      parameter_to_xml(distr$mean),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</Exponential>"))
  text
}

#' Converts a gamma distribution to XML
#' @param distr a gamma distibution,
#'   as created by \code{\link{create_gamma_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distr_to_xml_gamma <- function(
  distr
) {
  testit::assert(beautier::is_gamma_distr(distr))
  id <- beautier::get_distr_id(distr)
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<Gamma ",
    "id=\"Gamma.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent(
      parameter_to_xml(distr$alpha),
      n_spaces = 4
    )
  )
  text <- c(text,
    indent(
      parameter_to_xml(distr$beta),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</Gamma>"))
  text
}

#' Converts a inv_gamma distribution to XML
#' @param distr a inv_gamma distibution,
#'   as created by \code{\link{create_inv_gamma_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distr_to_xml_inv_gamma <- function(
  distr
) {
  testit::assert(beautier::is_inv_gamma_distr(distr))
  id <- beautier::get_distr_id(distr)
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<InverseGamma ",
    "id=\"InverseGamma.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent(
      parameter_to_xml(distr$alpha),
      n_spaces = 4
    )
  )
  text <- c(text,
    indent(
      parameter_to_xml(distr$beta),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</InverseGamma>"))
  text
}

#' Converts a laplace distibution to XML
#' @param distr a laplace distibution,
#'   as created by \code{\link{create_laplace_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distr_to_xml_laplace <- function(
  distr
) {
  testit::assert(beautier::is_laplace_distr(distr))
  id <- beautier::get_distr_id(distr)
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<LaplaceDistribution ",
    "id=\"LaplaceDistribution.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent(
      parameter_to_xml(distr$mu),
      n_spaces = 4
    )
  )
  text <- c(text,
    indent(
      parameter_to_xml(distr$scale),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</LaplaceDistribution>"))
  text
}
#' Converts a log-normal distribution to XML
#' @param distr a log-normal distibution,
#'   as created by \code{\link{create_log_normal_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distr_to_xml_log_normal <- function(
  distr
) {
  testit::assert(beautier::is_log_normal_distr(distr))
  id <- beautier::get_distr_id(distr)
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<LogNormal ",
    "id=\"LogNormalDistributionModel.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent(
      parameter_to_xml(distr$m),
      n_spaces = 4
    )
  )
  text <- c(text,
    indent(
      parameter_to_xml(distr$s),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</LogNormal>"))
  text
}


#' Converts a normal distribution to XML
#' @param distr a normal distibution,
#'   as created by \code{\link{create_normal_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distr_to_xml_normal <- function(
  distr
) {
  testit::assert(beautier::is_normal_distr(distr))
  id <- beautier::get_distr_id(distr)
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<Normal ",
    "id=\"Normal.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent(
      parameter_to_xml(distr$mean),
      n_spaces = 4
    )
  )
  text <- c(text,
    indent(
      parameter_to_xml(distr$sigma),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</Normal>"))
  text
}

#' Converts a 1/x distribution to XML
#' @param distr a 1/x distibution,
#'   as created by \code{\link{create_one_div_x_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distr_to_xml_one_div_x <- function(
  distr
) {
  testit::assert(beautier::is_one_div_x_distr(distr))
  id <- beautier::get_distr_id(distr)
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<OneOnX ",
    "id=\"OneOnX.", id, "\" name=\"distr\"/>"))
  text
}

#' Converts a poisson distribution to XML
#' @param distr a poisson distibution,
#'   as created by \code{\link{create_poisson_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distr_to_xml_poisson <- function(
  distr
) {
  testit::assert(beautier::is_poisson_distr(distr))
  id <- beautier::get_distr_id(distr)
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<distr ",
    "id=\"Poisson.", id, "\" ",
    "spec=\"beast.math.distributions.Poisson\">"))
  text <- c(text,
    indent(
      parameter_to_xml(distr$lambda),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</distr>"))
  text
}

#' Converts a uniform distribution to XML
#' @param distr a uniform distibution,
#'   as created by \code{\link{create_uniform_distr}})
#' @return the distribution as XML text
#' @author Richel J.C. Bilderbeek
distr_to_xml_uniform <- function(
  distr
) {
  testit::assert(beautier::is_uniform_distr(distr))
  id <- beautier::get_distr_id(distr)
  testit::assert(beautier::is_id(id))

  text <- NULL
  line_begin <- paste0("<Uniform id=\"Uniform.", id, "\" name=\"distr\"")
  line_end <- "/>"
  upper <- distr$upper
  if (is.na(upper)) {
    text <- c(text, paste0(line_begin, line_end))
  } else if (is.infinite(upper)) {
    text <- c(text, paste0(line_begin, " upper=\"Infinity\"", line_end))
  } else {
    text <- c(text, paste0(line_begin, " upper=\"", upper, "\"", line_end))
  }
  text
}