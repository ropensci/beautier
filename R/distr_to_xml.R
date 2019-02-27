#' Converts a distribution to XML
#' @param distr a distribution,
#'   as created by \code{\link{create_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   xml <- beautier:::distr_to_xml(create_uniform_distr(id = 1))
#'   testit::assert(is.character(xml))
#'   testit::assert(length(xml) == 1)
#'   testit::assert(nchar(xml) > 1)
#' @noRd
distr_to_xml <- function(
  distr
) {
  text <- NULL
  id <- distr$id
  if (!is_id(id)) { # nolint beautier function
    stop("distribution must have an ID")
  }
  if (is_beta_distr(distr)) { # nolint beautier function
    text <- c(text, distr_to_xml_beta(distr)) # nolint beautier function
  } else if (is_exp_distr(distr)) { # nolint beautier function
    text <- c(text, distr_to_xml_exp(distr)) # nolint beautier function
  } else if (is_gamma_distr(distr)) { # nolint beautier function
    text <- c(text, distr_to_xml_gamma(distr)) # nolint beautier function
  } else if (is_inv_gamma_distr(distr)) { # nolint beautier function
    text <- c(text, distr_to_xml_inv_gamma(distr)) # nolint beautier function
  } else if (is_laplace_distr(distr)) { # nolint beautier function
    text <- c(text, distr_to_xml_laplace(distr)) # nolint beautier function
  } else if (is_log_normal_distr(distr)) { # nolint beautier function
    text <- c(text, distr_to_xml_log_normal(distr)) # nolint beautier function
  } else if (is_normal_distr(distr)) { # nolint beautier function
    text <- c(text, distr_to_xml_normal(distr)) # nolint beautier function
  } else if (is_one_div_x_distr(distr)) { # nolint beautier function
    text <- c(text, distr_to_xml_one_div_x(distr)) # nolint beautier function
  } else if (is_poisson_distr(distr)) { # nolint beautier function
    text <- c(text, distr_to_xml_poisson(distr)) # nolint beautier function
  } else {
    testit::assert(is_uniform_distr(distr)) # nolint beautier function
    text <- c(text, distr_to_xml_uniform(distr)) # nolint beautier function
  }
  testit::assert(is_xml(text)) # nolint
  text
}

#' Converts a beta distribution to XML
#' @param distr a beta distribution,
#'   as created by \code{\link{create_beta_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
distr_to_xml_beta <- function(
  distr
) {
  testit::assert(is_beta_distr(distr)) # nolint beautier function
  id <- distr$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  text <- c(text, paste0("<Beta id=\"Beta.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$alpha), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$beta), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</Beta>"))
  text
}

#' Converts an exponential distribution to XML
#' @param distr an exponential distribution,
#'   as created by \code{\link{create_exp_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
distr_to_xml_exp <- function(
  distr
) {
  testit::assert(is_exp_distr(distr)) # nolint beautier function
  id <- distr$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  text <- c(text, paste0("<Exponential ",
    "id=\"Exponential.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$mean), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</Exponential>"))
  text
}

#' Converts a gamma distribution to XML
#' @param distr a gamma distribution,
#'   as created by \code{\link{create_gamma_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
distr_to_xml_gamma <- function(
  distr
) {
  testit::assert(is_gamma_distr(distr)) # nolint beautier function
  id <- distr$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  text <- c(text, paste0("<Gamma ",
    "id=\"Gamma.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$alpha), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$beta), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</Gamma>"))
  text
}

#' Converts an inverse-gamma distribution to XML
#' @param distr an inverse-gamma distribution,
#'   as created by \code{\link{create_inv_gamma_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
distr_to_xml_inv_gamma <- function(
  distr
) {
  testit::assert(is_inv_gamma_distr(distr)) # nolint beautier function
  id <- distr$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  text <- c(text, paste0("<InverseGamma ",
    "id=\"InverseGamma.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$alpha), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$beta), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</InverseGamma>"))
  text
}

#' Converts a Laplace distribution to XML
#' @param distr a Laplace distribution
#'   as created by \code{\link{create_laplace_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
distr_to_xml_laplace <- function(
  distr
) {
  testit::assert(is_laplace_distr(distr)) # nolint beautier function
  id <- distr$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  text <- c(text, paste0("<LaplaceDistribution ",
    "id=\"LaplaceDistribution.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$mu), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$scale), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</LaplaceDistribution>"))
  text
}
#' Converts a log-normal distribution to XML
#' @param distr a log-normal distribution,
#'   as created by \code{\link{create_log_normal_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
distr_to_xml_log_normal <- function(
  distr
) {
  testit::assert(is_log_normal_distr(distr)) # nolint beautier function
  id <- distr$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  text <- c(text, paste0("<LogNormal ",
    "id=\"LogNormalDistributionModel.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$m), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$s), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</LogNormal>"))
  text
}


#' Converts a normal distribution to XML
#' @param distr a normal distribution,
#'   as created by \code{\link{create_normal_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
distr_to_xml_normal <- function(
  distr
) {
  testit::assert(is_normal_distr(distr)) # nolint beautier function
  id <- distr$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  text <- c(text, paste0("<Normal ",
    "id=\"Normal.", id, "\" name=\"distr\">"))
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$mean), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$sigma), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</Normal>"))
  text
}

#' Converts a 1/x distribution to XML
#' @param distr a 1/x distribution,
#'   as created by \code{\link{create_one_div_x_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
distr_to_xml_one_div_x <- function(
  distr
) {
  testit::assert(is_one_div_x_distr(distr)) # nolint beautier function
  id <- distr$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  text <- c(text, paste0("<OneOnX ",
    "id=\"OneOnX.", id, "\" name=\"distr\"/>")) # nolint this is no absolute path
  text
}

#' Converts a Poisson distribution to XML
#' @param distr a Poisson distribution,
#'   as created by \code{\link{create_poisson_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
distr_to_xml_poisson <- function(
  distr
) {
  testit::assert(is_poisson_distr(distr)) # nolint beautier function
  id <- distr$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  text <- c(text, paste0("<distr ",
    "id=\"Poisson.", id, "\" ",
    "spec=\"beast.math.distributions.Poisson\">"))
  text <- c(text,
    indent( # nolint beautier function
      parameter_to_xml(distr$lambda), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</distr>"))
  text
}

#' Converts a uniform distribution to XML
#' @param distr a uniform distribution,
#'   as created by \code{\link{create_uniform_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
distr_to_xml_uniform <- function(
  distr
) {
  testit::assert(is_uniform_distr(distr)) # nolint beautier function
  id <- distr$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  line_begin <- paste0("<Uniform id=\"Uniform.", id, "\" name=\"distr\"")
  line_end <- "/>" # nolint this is no absolute path
  upper <- distr$upper
  if (is_one_na(upper)) { # nolint beautier function
    text <- c(text, paste0(line_begin, line_end))
  } else if (is.infinite(upper)) {
    text <- c(text, paste0(line_begin, " upper=\"Infinity\"", line_end))
  } else {
    text <- c(text, paste0(line_begin, " upper=\"", upper, "\"", line_end))
  }
  text
}
