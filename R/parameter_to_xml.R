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
  if (is.na(id)) {
    stop("parameter must have an ID")
  }
  if (is_alpha_parameter(parameter)) {
    text <- c(text, parameter_to_xml_alpha(parameter))
  } else if (is_beta_parameter(parameter)) {
    text <- c(text, parameter_to_xml_beta(parameter))
  } else if (is_lambda_parameter(parameter)) {
    text <- c(text, parameter_to_xml_lambda(parameter))
  } else if (is_m_parameter(parameter)) {
    text <- c(text, parameter_to_xml_m(parameter))
  } else if (is_mean_parameter(parameter)) {
    text <- c(text, parameter_to_xml_mean(parameter))
  } else if (is_mu_parameter(parameter)) {
    text <- c(text, parameter_to_xml_mu(parameter))
  } else if (is_s_parameter(parameter)) {
    text <- c(text, parameter_to_xml_s(parameter))
  } else if (is_scale_parameter(parameter)) {
    text <- c(text, parameter_to_xml_scale(parameter))
  } else {
    testit::assert(is_sigma_parameter(parameter))
    text <- c(text, parameter_to_xml_sigma(parameter))
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
  testit::assert(is_alpha_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(!is.na(id))
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
  testit::assert(is_beta_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(!is.na(id))
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

#' Converts a lambda parameter to XML
#' @param parameter a lambda parameter,
#'   as created by \code{\link{create_lambda_parameter}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_lambda <- function(
  parameter
) {
  testit::assert(is_lambda_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(!is.na(id))
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
  testit::assert(is_m_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(!is.na(id))
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
  testit::assert(is_mean_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(!is.na(id))
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
  testit::assert(is_mu_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(!is.na(id))
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
  testit::assert(is_s_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(!is.na(id))
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
  testit::assert(is_scale_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(!is.na(id))
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
  testit::assert(is_sigma_parameter(parameter))
  id <- beautier::get_parameter_id(parameter)
  testit::assert(!is.na(id))
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
