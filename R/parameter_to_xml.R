#' Converts a parameter to XML
#' @param parameter a distibution,
#'   as created by \code{\link{create_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
#' @export
parameter_to_xml <- function(
  parameter
) {
  text <- NULL
  id <- beautier::get_parameter_id(parameter)
  if (!beautier::is_id(id)) {
    stop("parameter must have an ID")
  }
  if (beautier::is_alpha_parameter(parameter)) {
    text <- c(text, parameter_to_xml_alpha(parameter)) # nolint internal function
  } else if (beautier::is_beta_parameter(parameter)) {
    text <- c(text, parameter_to_xml_beta(parameter)) # nolint internal function
  } else if (beautier::is_clock_rate_parameter(parameter)) {
    text <- c(text, parameter_to_xml_clock_rate(parameter)) # nolint internal function
  } else if (beautier::is_lambda_parameter(parameter)) {
    text <- c(text, parameter_to_xml_lambda(parameter)) # nolint internal function
  } else if (beautier::is_m_parameter(parameter)) {
    text <- c(text, parameter_to_xml_m(parameter)) # nolint internal function
  } else if (beautier::is_mean_parameter(parameter)) {
    text <- c(text, parameter_to_xml_mean(parameter)) # nolint internal function
  } else if (beautier::is_mu_parameter(parameter)) {
    text <- c(text, parameter_to_xml_mu(parameter)) # nolint internal function
  } else if (beautier::is_s_parameter(parameter)) {
    text <- c(text, parameter_to_xml_s(parameter)) # nolint internal function
  } else if (beautier::is_scale_parameter(parameter)) {
    text <- c(text, parameter_to_xml_scale(parameter)) # nolint internal function
  } else {
    testit::assert(beautier::is_sigma_parameter(parameter))
    text <- c(text, parameter_to_xml_sigma(parameter)) # nolint internal function
  }
  text
}

#' Converts an alpha parameter to XML
#' @param parameter an alpha parameter,
#'   as created by \code{\link{create_alpha_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_alpha <- function(
  parameter
) {
  testit::assert(beautier::is_alpha_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(beautier::is_id(id))
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value

  text <- NULL
  text <- c(text,
    paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "estimate=\"", estimate, "\" ",
      "name=\"alpha\">", value,
      "</parameter>"
    )
  )
  text
}

#' Converts a beta parameter to XML
#' @param parameter a beta parameter,
#'   as created by \code{\link{create_beta_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_beta <- function(
  parameter
) {
  testit::assert(beautier::is_beta_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(beautier::is_id(id))
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  text <- NULL
  text <- c(text,
    paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "estimate=\"", estimate, "\" ",
      "name=\"beta\">", value,
      "</parameter>"
    )
  )
  text
}

#' Converts a \code{clockRate} parameter to XML
#' @param parameter a \code{clockRate} parameter,
#'   as created by \code{\link{create_clock_rate_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_clock_rate <- function(
  parameter
) {
  testit::assert(beautier::is_clock_rate_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(beautier::is_id(id))
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  text <- NULL
  text <- c(
    text,
    paste0(
      "<parameter ",
      "id=\"clockRate.c:", id, "\" ",
      "estimate=\"", estimate, "\" ",
      "name=\"clock.rate\">",
      value,
      "</parameter>"
    )
  )
  text
}

#' Converts a lambda parameter to XML
#' @param parameter a lambda parameter,
#'   as created by \code{\link{create_lambda_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_lambda <- function(
  parameter
) {
  testit::assert(beautier::is_lambda_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(beautier::is_id(id))
  value <- parameter$value
  text <- NULL
  text <- c(text,
    paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "name=\"lambda\">", value,
      "</parameter>"
    )
  )
  text
}

#' Converts a m parameter to XML
#' @param parameter a m parameter,
#'   as created by \code{\link{create_m_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_m <- function(
  parameter
) {
  testit::assert(beautier::is_m_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(beautier::is_id(id))
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  text <- NULL
  text <- c(text,
    paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "estimate=\"", estimate, "\" ",
      "name=\"M\">", value,
      "</parameter>"
    )
  )
  text
}

#' Converts a mean parameter to XML
#' @param parameter a mean parameter,
#'   as created by \code{\link{create_mean_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_mean <- function(
  parameter
) {
  testit::assert(beautier::is_mean_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(beautier::is_id(id))
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  text <- NULL
  text <- c(text,
    paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "estimate=\"", estimate, "\" ",
      "name=\"mean\">", value,
      "</parameter>"
    )
  )
  text
}

#' Converts a mu parameter to XML
#' @param parameter a mu parameter,
#'   as created by \code{\link{create_mu_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_mu <- function(
  parameter
) {
  testit::assert(beautier::is_mu_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(beautier::is_id(id))
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  text <- NULL
  text <- c(text,
    paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "estimate=\"", estimate, "\" ",
      "name=\"mu\">", value,
      "</parameter>"
    )
  )
  text
}

#' Converts a s parameter to XML
#' @param parameter a s parameter,
#'   as created by \code{\link{create_s_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_s <- function(
  parameter
) {
  testit::assert(beautier::is_s_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(beautier::is_id(id))
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  lower <- parameter$lower
  upper <- parameter$upper
  text <- NULL
  text <- c(text,
    paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "estimate=\"", estimate, "\" ",
      "lower=\"", lower, "\" ",
      "name=\"S\" ",
      "upper=\"", upper, "\">", value,
      "</parameter>"
    )
  )
  text
}

#' Converts a scale parameter to XML
#' @param parameter a scale parameter,
#'   as created by \code{\link{create_scale_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_scale <- function(
  parameter
) {
  testit::assert(beautier::is_scale_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(beautier::is_id(id))
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  text <- NULL
  text <- c(text,
    paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "estimate=\"", estimate, "\" ",
      "name=\"scale\">", value,
      "</parameter>"
    )
  )
  text
}

#' Converts a sigma parameter to XML
#' @param parameter a sigma parameter,
#'   as created by \code{\link{create_sigma_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_sigma <- function(
  parameter
) {
  testit::assert(beautier::is_sigma_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(beautier::is_id(id))
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  text <- NULL
  text <- c(text,
    paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "estimate=\"", estimate, "\" ",
      "name=\"sigma\">", value,
      "</parameter>"
    )
  )
  text
}
