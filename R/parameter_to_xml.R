#' Converts a parameter to XML
#' @param parameter a parameter,
#'   as created by \code{\link{create_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   xml <- beautier:::parameter_to_xml(create_alpha_param(id = 1))
#'   testit::assert(length(xml) == 1)
#'   testit::assert(nchar(xml) > 1)
#' @noRd
parameter_to_xml <- function(
  parameter
) {
  text <- NULL
  id <- parameter$id
  if (!is_id(id)) { # nolint beautier function
    stop("parameter must have an ID")
  }
  testit::assert(!is_one_na(id)) # nolint beautier function
  if (is_one_na(parameter$value)) { # nolint beautier function
    stop("parameter must have a value")
  }
  if (is_alpha_param(parameter)) {
    text <- c(text, parameter_to_xml_alpha(parameter)) # nolint beautier function
  } else if (is_beta_param(parameter)) {
    text <- c(text, parameter_to_xml_beta(parameter)) # nolint beautier function
  } else if (is_clock_rate_param(parameter)) {
    text <- c(text, parameter_to_xml_clock_rate(parameter)) # nolint beautier function
  } else if (is_kappa_1_param(parameter)) {
    text <- c(text, parameter_to_xml_kappa_1(parameter)) # nolint beautier function
  } else if (is_kappa_2_param(parameter)) {
    text <- c(text, parameter_to_xml_kappa_2(parameter)) # nolint beautier function
  } else if (is_lambda_param(parameter)) {
    text <- c(text, parameter_to_xml_lambda(parameter)) # nolint beautier function
  } else if (is_m_param(parameter)) {
    text <- c(text, parameter_to_xml_m(parameter)) # nolint beautier function
  } else if (is_mean_param(parameter)) {
    text <- c(text, parameter_to_xml_mean(parameter)) # nolint beautier function
  } else if (is_mu_param(parameter)) {
    text <- c(text, parameter_to_xml_mu(parameter)) # nolint beautier function
  } else if (is_rate_ac_param(parameter)) {
    text <- c(text, parameter_to_xml_rate_ac(parameter)) # nolint beautier function
  } else if (is_rate_ag_param(parameter)) {
    text <- c(text, parameter_to_xml_rate_ag(parameter)) # nolint beautier function
  } else if (is_rate_at_param(parameter)) {
    text <- c(text, parameter_to_xml_rate_at(parameter)) # nolint beautier function
  } else if (is_rate_cg_param(parameter)) {
    text <- c(text, parameter_to_xml_rate_cg(parameter)) # nolint beautier function
  } else if (is_rate_ct_param(parameter)) {
    text <- c(text, parameter_to_xml_rate_ct(parameter)) # nolint beautier function
  } else if (is_rate_gt_param(parameter)) {
    text <- c(text, parameter_to_xml_rate_gt(parameter)) # nolint beautier function
  } else if (is_s_param(parameter)) {
    text <- c(text, parameter_to_xml_s(parameter)) # nolint beautier function
  } else if (is_scale_param(parameter)) {
    text <- c(text, parameter_to_xml_scale(parameter)) # nolint beautier function
  } else {
    # This assert will also fail for new parameter types
    testit::assert(is_sigma_param(parameter)) # nolint beautier function
    text <- c(text, parameter_to_xml_sigma(parameter)) # nolint beautier function
  }
  testit::assert(is_xml(text)) # nolint
  text
}

#' Converts an alpha parameter to XML
#' @param parameter an alpha parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_alpha_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_alpha <- function(
  parameter
) {
  testit::assert(is_alpha_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\" ",
    "estimate=\"", estimate, "\" ",
    "name=\"alpha\">", parameter$value,
    "</parameter>"
  )
}

#' Converts a beta parameter to XML
#' @param parameter a beta parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_beta_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_beta <- function(
  parameter
) {
  testit::assert(is_beta_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\" ",
    "estimate=\"", estimate, "\" ",
    "name=\"beta\">", parameter$value,
    "</parameter>"
  )
}

#' Converts a \code{clockRate} parameter to XML
#' @param parameter a \code{clockRate} parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_clock_rate_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_clock_rate <- function(
  parameter
) {
  testit::assert(is_clock_rate_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  paste0(
    "<parameter ",
    "id=\"clockRate.c:", id, "\" ",
    "estimate=\"", estimate, "\" ",
    "name=\"clock.rate\">",
    parameter$value,
    "</parameter>"
  )
}

#' Converts a kappa 1 parameter to XML
#' @param parameter a kappa 1 parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_kappa_1_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_kappa_1 <- function(
  parameter
) {
  testit::assert(is_kappa_1_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  paste0("<parameter id=\"kappa1.s:", id, "\" ",
    "lower=\"", parameter$lower, "\" ",
    "name=\"stateNode\">", parameter$value, "</parameter>"
  )
}

#' Converts a kappa 2 parameter to XML
#' @param parameter a kappa 2 parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_kappa_2_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_kappa_2 <- function(
  parameter
) {
  testit::assert(is_kappa_2_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  paste0("<parameter id=\"kappa2.s:", id, "\" ",
    "lower=\"", parameter$lower, "\" ",
    "name=\"stateNode\">", parameter$value, "</parameter>"
  )
}



#' Converts a lambda parameter to XML
#' @param parameter a lambda parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_lambda_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_lambda <- function(
  parameter
) {
  testit::assert(is_lambda_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\" ",
    "name=\"lambda\">", parameter$value,
    "</parameter>"
  )
}

#' Converts a m parameter to XML
#' @param parameter a m parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_m_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_m <- function(
  parameter
) {
  testit::assert(is_m_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\" ",
    "estimate=\"", estimate, "\" ",
    "name=\"M\">", parameter$value,
    "</parameter>"
  )
}

#' Converts a mean parameter to XML
#' @param parameter a mean parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_mean_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_mean <- function(
  parameter
) {
  testit::assert(is_mean_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\" ",
    "estimate=\"", estimate, "\" ",
    "name=\"mean\">", parameter$value,
    "</parameter>"
  )
}

#' Converts a mu parameter to XML
#' @param parameter a mu parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_mu_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_mu <- function(
  parameter
) {
  testit::assert(is_mu_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\" ",
    "estimate=\"", estimate, "\" ",
    "name=\"mu\">", parameter$value,
    "</parameter>"
  )
}

#' Converts a 'rate AC' parameter to XML
#' @param parameter a 'rate AC' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_ac_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_rate_ac <- function(
  parameter,
  which_name = "state_node"
) {
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(is_rate_ac_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  value <- parameter$value
  lower <- parameter$lower
  line <- paste0("<parameter id=\"rateAC.s:", id, "\"")
  if (parameter$estimate == FALSE) {
    line <- paste0(line, " estimate=\"false\"")
  }
  name_str <- NULL
  if (which_name == "state_node") name_str <- "stateNode"
  if (which_name == "rate_name") name_str <- "rateAC"
  testit::assert(!is.null(name_str))

  paste0(line, " lower=\"", lower, "\"",
    " name=\"", name_str, "\">", value, "</parameter>"
  )
}

#' Converts a 'rate AG' parameter to XML
#' @param parameter a 'rate AG' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_ag_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_rate_ag <- function(
  parameter,
  which_name = "state_node"
) {
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(is_rate_ag_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  value <- parameter$value
  lower <- parameter$lower
  line <- paste0("<parameter id=\"rateAG.s:", id, "\"")
  if (parameter$estimate == FALSE) {
    line <- paste0(line, " estimate=\"false\"")
  }
  name_str <- NULL
  if (which_name == "state_node") name_str <- "stateNode"
  if (which_name == "rate_name") name_str <- "rateAG"
  testit::assert(!is.null(name_str))

  paste0(line, " lower=\"", lower, "\"",
    " name=\"", name_str, "\">", value, "</parameter>"
  )
}

#' Converts a 'rate AT' parameter to XML
#' @param parameter a 'rate AT' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_at_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_rate_at <- function(
  parameter,
  which_name = "state_node"
) {
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(is_rate_at_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  value <- parameter$value
  lower <- parameter$lower
  line <- paste0("<parameter id=\"rateAT.s:", id, "\"")
  if (parameter$estimate == FALSE) {
    line <- paste0(line, " estimate=\"false\"")
  }
  name_str <- NULL
  if (which_name == "state_node") name_str <- "stateNode"
  if (which_name == "rate_name") name_str <- "rateAT"
  testit::assert(!is.null(name_str))

  paste0(line, " lower=\"", lower, "\"",
    " name=\"", name_str, "\">", value, "</parameter>"
  )
}

#' Converts a 'rate CG' parameter to XML
#' @param parameter a 'rate CG' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_cg_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_rate_cg <- function(
  parameter,
  which_name = "state_node"
) {
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(is_rate_cg_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  value <- parameter$value
  lower <- parameter$lower
  line <- paste0("<parameter id=\"rateCG.s:", id, "\"")
  if (parameter$estimate == FALSE) {
    line <- paste0(line, " estimate=\"false\"")
  }
  name_str <- NULL
  if (which_name == "state_node") name_str <- "stateNode"
  if (which_name == "rate_name") name_str <- "rateCG"
  testit::assert(!is.null(name_str))

  paste0(line, " lower=\"", lower, "\"",
    " name=\"", name_str, "\">", value, "</parameter>"
  )
}

#' Converts a 'rate CT' parameter to XML
#' @param parameter a 'rate CT' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_ct_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_rate_ct <- function(
  parameter,
  which_name = "state_node"
) {
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(is_rate_ct_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  value <- parameter$value
  lower <- parameter$lower
  line <- paste0("<parameter id=\"rateCT.s:", id, "\"")
  if (parameter$estimate == FALSE) {
    line <- paste0(line, " estimate=\"false\"")
  }
  name_str <- NULL
  if (which_name == "state_node") name_str <- "stateNode"
  if (which_name == "rate_name") name_str <- "rateCT"
  testit::assert(!is.null(name_str))

  paste0(line, " lower=\"", lower, "\"",
    " name=\"", name_str, "\">", value, "</parameter>"
  )
}

#' Converts a 'rate GT' parameter to XML
#' @param parameter a 'rate GT' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_gt_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_rate_gt <- function(
  parameter,
  which_name = "state_node"
) {
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(is_rate_gt_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  value <- parameter$value
  lower <- parameter$lower
  line <- paste0("<parameter id=\"rateGT.s:", id, "\"")
  if (parameter$estimate == FALSE) {
    line <- paste0(line, " estimate=\"false\"")
  }
  name_str <- NULL
  if (which_name == "state_node") name_str <- "stateNode"
  if (which_name == "rate_name") name_str <- "rateGT"
  testit::assert(!is.null(name_str))

  paste0(line, " lower=\"", lower, "\"",
    " name=\"", name_str, "\">", value, "</parameter>"
  )
}

#' Converts a s parameter to XML
#' @param parameter a s parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_s_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_s <- function(
  parameter
) {
  testit::assert(is_s_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  value <- parameter$value
  lower <- parameter$lower
  upper <- parameter$upper
  text <- paste0(
      "<parameter ",
      "id=\"RealParameter.", id, "\" ",
      "estimate=\"", estimate, "\""
    )
  if (!is_one_na(lower)) { # nolint beautier function
    text <- paste0(text, " lower=\"", lower, "\"")
  }
  text <- paste0(text, " name=\"S\"")
  if (!is_one_na(upper)) { # nolint beautier function
    upper_txt <- upper
    if (is.infinite(upper)) {
      upper_txt <- "Infinity"
    }
    text <- paste0(text, " upper=\"", upper_txt, "\"")
  }
  text <- paste0(text, ">", value, "</parameter>")
  text
}

#' Converts a scale parameter to XML
#' @param parameter a scale parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_scale_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_scale <- function(
  parameter
) {
  testit::assert(is_scale_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\" ",
    "estimate=\"", estimate, "\" ",
    "name=\"scale\">", parameter$value,
    "</parameter>"
  )
}

#' Converts a sigma parameter to XML
#' @param parameter a sigma parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_sigma_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
parameter_to_xml_sigma <- function(
  parameter
) {
  testit::assert(is_sigma_param(parameter)) # nolint beautier function
  id <- parameter$id
  testit::assert(is_id(id)) # nolint beautier function
  testit::assert(parameter$estimate == FALSE)
  estimate <- ifelse(parameter$estimate == TRUE, "true", "false")
  paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\" ",
    "estimate=\"", estimate, "\" ",
    "name=\"sigma\">", parameter$value,
    "</parameter>"
  )
}
