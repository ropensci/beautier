#' Converts a parameter to XML
#' @param parameter a distibution,
#'   as created by \code{\link{createparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
#' @export
parameter_to_xml <- function(
  parameter
) {
  text <- NULL
  id <- beautier::getparam_id(parameter)
  if (!beautier::is_id(id)) {
    stop("parameter must have an ID")
  }
  testit::assert(!is.na(id))
  if (is.na(parameter$value)) {
    stop("parameter must have a value")
  }
  if (beautier::is_alphaparam(parameter)) {
    text <- c(text, parameter_to_xml_alpha(parameter)) # nolint internal function
  } else if (beautier::is_betaparam(parameter)) {
    text <- c(text, parameter_to_xml_beta(parameter)) # nolint internal function
  } else if (beautier::is_clock_rateparam(parameter)) {
    text <- c(text, parameter_to_xml_clock_rate(parameter)) # nolint internal function
  } else if (beautier::is_lambdaparam(parameter)) {
    text <- c(text, parameter_to_xml_lambda(parameter)) # nolint internal function
  } else if (beautier::is_mparam(parameter)) {
    text <- c(text, parameter_to_xml_m(parameter)) # nolint internal function
  } else if (beautier::is_meanparam(parameter)) {
    text <- c(text, parameter_to_xml_mean(parameter)) # nolint internal function
  } else if (beautier::is_muparam(parameter)) {
    text <- c(text, parameter_to_xml_mu(parameter)) # nolint internal function
  } else if (beautier::is_sparam(parameter)) {
    text <- c(text, parameter_to_xml_s(parameter)) # nolint internal function
  } else if (beautier::is_scaleparam(parameter)) {
    text <- c(text, parameter_to_xml_scale(parameter)) # nolint internal function
  } else {
    testit::assert(beautier::is_sigmaparam(parameter))
    text <- c(text, parameter_to_xml_sigma(parameter)) # nolint internal function
  }
  text
}

#' Converts an alpha parameter to XML
#' @param parameter an alpha parameter,
#'   as created by \code{\link{create_alphaparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_alpha <- function(
  parameter
) {
  testit::assert(beautier::is_alphaparam(parameter))
  id <- beautier::getparam_id(parameter)
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
#'   as created by \code{\link{create_betaparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_beta <- function(
  parameter
) {
  testit::assert(beautier::is_betaparam(parameter))
  id <- beautier::getparam_id(parameter)
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
#'   as created by \code{\link{create_clock_rateparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_clock_rate <- function(
  parameter
) {
  testit::assert(beautier::is_clock_rateparam(parameter))
  id <- beautier::getparam_id(parameter)
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
#'   as created by \code{\link{create_lambdaparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_lambda <- function(
  parameter
) {
  testit::assert(beautier::is_lambdaparam(parameter))
  id <- beautier::getparam_id(parameter)
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
#'   as created by \code{\link{create_mparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_m <- function(
  parameter
) {
  testit::assert(beautier::is_mparam(parameter))
  id <- beautier::getparam_id(parameter)
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
#'   as created by \code{\link{create_meanparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_mean <- function(
  parameter
) {
  testit::assert(beautier::is_meanparam(parameter))
  id <- beautier::getparam_id(parameter)
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
#'   as created by \code{\link{create_muparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_mu <- function(
  parameter
) {
  testit::assert(beautier::is_muparam(parameter))
  id <- beautier::getparam_id(parameter)
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
#'   as created by \code{\link{create_sparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_s <- function(
  parameter
) {
  testit::assert(beautier::is_sparam(parameter))
  id <- beautier::getparam_id(parameter)
  testit::assert(beautier::is_id(id))
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  lower <- parameter$lower
  upper <- parameter$upper
  text <- paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "estimate=\"", estimate, "\""
    )
  if (!is.na(lower)) {
    text <- paste0(text, " lower=\"", lower, "\"")
  }
  text <- paste0(text, " name=\"S\"")
  if (!is.na(upper)) {
    text <- paste0(text, " upper=\"", upper, "\"")
  }
  text <- paste0(text, ">", value, "</parameter>")
  text
}

#' Converts a scale parameter to XML
#' @param parameter a scale parameter,
#'   as created by \code{\link{create_scaleparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_scale <- function(
  parameter
) {
  testit::assert(beautier::is_scaleparam(parameter))
  id <- beautier::getparam_id(parameter)
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
#'   as created by \code{\link{create_sigmaparam}})
#' @return the parameter as XML text
#' @author Richel J.C. Bilderbeek
parameter_to_xml_sigma <- function(
  parameter
) {
  testit::assert(beautier::is_sigmaparam(parameter))
  id <- beautier::getparam_id(parameter)
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
