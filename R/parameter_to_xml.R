#' Internal function
#'
#' Converts a parameter to XML
#' @param parameter a parameter,
#'   as created by \code{\link{create_param}})
#' @inheritParams default_params_doc
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' parameter_to_xml(create_alpha_param(id = 1))
#'
#' check_empty_beautier_folder()
#' @export
parameter_to_xml <- function( # nolint simplifying further hurts readability
  parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  beautier::check_param(parameter)
  testit::assert(beautier::is_id(parameter$id))
  if (beautier::is_alpha_param(parameter)) {
    return(beautier::alpha_parameter_to_xml(alpha_parameter = parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_beta_param(parameter)) {
    return(beautier::beta_parameter_to_xml(beta_parameter = parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_clock_rate_param(parameter)) {
    return(beautier::clock_rate_param_to_xml(
      clock_rate_param = parameter,
      beauti_options = beauti_options
    ))
  } else if (beautier::is_kappa_1_param(parameter)) {
    return(beautier::parameter_to_xml_kappa_1(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_kappa_2_param(parameter)) {
    return(beautier::parameter_to_xml_kappa_2(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_lambda_param(parameter)) {
    return(beautier::parameter_to_xml_lambda(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_m_param(parameter)) {
    return(beautier::m_param_to_xml(m_param = parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_mean_param(parameter)) {
    return(beautier::parameter_to_xml_mean(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_mu_param(parameter)) {
    return(beautier::parameter_to_xml_mu(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_rate_ac_param(parameter)) {
    return(beautier::parameter_to_xml_rate_ac(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_rate_ag_param(parameter)) {
    return(beautier::parameter_to_xml_rate_ag(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_rate_at_param(parameter)) {
    return(beautier::parameter_to_xml_rate_at(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_rate_cg_param(parameter)) {
    return(beautier::parameter_to_xml_rate_cg(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_rate_ct_param(parameter)) {
    return(beautier::parameter_to_xml_rate_ct(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_rate_gt_param(parameter)) {
    return(beautier::parameter_to_xml_rate_gt(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_s_param(parameter)) {
    return(beautier::parameter_to_xml_s(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_scale_param(parameter)) {
    return(beautier::parameter_to_xml_scale(parameter, beauti_options = beauti_options)) # nolint indeed a long line
  }
  # This assert will also fail for new parameter types
  testthat::expect_true(beautier::is_sigma_param(parameter))
  beautier::parameter_to_xml_sigma(parameter, beauti_options = beauti_options)
}

#' Internal function
#'
#' Converts a kappa 1 parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a kappa 1 parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_kappa_1_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_kappa_1 <- function(
  parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_kappa_1_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
  paste0("<parameter id=\"kappa1.s:", id, "\" ",
    "lower=\"", parameter$lower, "\" ",
    "name=\"stateNode\">", parameter$value, "</parameter>"
  )
}

#' Internal function
#'
#' Converts a kappa 2 parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a kappa 2 parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_kappa_2_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_kappa_2 <- function(
  parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_kappa_2_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
  paste0("<parameter id=\"kappa2.s:", id, "\" ",
    "lower=\"", parameter$lower, "\" ",
    "name=\"stateNode\">", parameter$value, "</parameter>"
  )
}



#' Internal function
#'
#' Converts a lambda parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a lambda parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_lambda_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_lambda <- function(
  parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_lambda_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
  paste0(
    "<parameter ",
    "id=\"RealParameter.", id, "\" ",
    "name=\"lambda\">", parameter$value,
    "</parameter>"
  )
}

#' Internal function
#'
#' Converts a mean parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a mean parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_mean_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_mean <- function(
  parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_mean_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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

#' Internal function
#'
#' Converts a mu parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a mu parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_mu_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_mu <- function(
  parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_mu_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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

#' Internal function
#'
#' Converts a 'rate AC' parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a 'rate AC' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_ac_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_rate_ac <- function(
  parameter,
  beauti_options = create_beauti_options(),
  which_name = "state_node"
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(beautier::is_rate_ac_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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

#' Internal function
#'
#' Converts a 'rate AG' parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a 'rate AG' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_ag_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_rate_ag <- function(
  parameter,
  beauti_options = create_beauti_options(),
  which_name = "state_node"
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(beautier::is_rate_ag_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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

#' Internal function
#'
#' Converts a 'rate AT' parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a 'rate AT' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_at_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_rate_at <- function(
  parameter,
  beauti_options = create_beauti_options(),
  which_name = "state_node"
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(beautier::is_rate_at_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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

#' Internal function
#'
#' Converts a 'rate CG' parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a 'rate CG' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_cg_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_rate_cg <- function(
  parameter,
  beauti_options = create_beauti_options(),
  which_name = "state_node"
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(beautier::is_rate_cg_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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

#' Internal function
#'
#' Converts a 'rate CT' parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a 'rate CT' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_ct_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_rate_ct <- function(
  parameter,
  beauti_options = create_beauti_options(),
  which_name = "state_node"
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(beautier::is_rate_ct_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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

#' Internal function
#'
#' Converts a 'rate GT' parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a 'rate GT' parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_rate_gt_param}})
#' @param which_name the name, can be \code{state_node} or \code{rate_name}
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_rate_gt <- function(
  parameter,
  beauti_options = create_beauti_options(),
  which_name = "state_node"
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(which_name %in% c("state_node", "rate_name"))
  testit::assert(beautier::is_rate_gt_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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

#' Internal function
#'
#' Converts a s parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a s parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_s_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_s <- function(
  parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_s_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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
  if (!beautier::is_one_na(lower)) {
    text <- paste0(text, " lower=\"", lower, "\"")
  }
  text <- paste0(text, " name=\"S\"")
  if (!beautier::is_one_na(upper)) {
    upper_txt <- upper
    if (is.infinite(upper)) {
      upper_txt <- "Infinity"
    }
    text <- paste0(text, " upper=\"", upper_txt, "\"")
  }
  text <- paste0(text, ">", value, "</parameter>")
  text
}

#' Internal function
#'
#' Converts a scale parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a scale parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_scale_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_scale <- function(
  parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_scale_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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

#' Internal function
#'
#' Converts a sigma parameter to XML
#' @inheritParams default_params_doc
#' @param parameter a sigma parameter,
#'   a numeric value.
#'   For advanced usage, use the structure
#'   as created by \code{\link{create_sigma_param}})
#' @return the parameter as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
parameter_to_xml_sigma <- function(
  parameter,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  testit::assert(beautier::is_sigma_param(parameter))
  id <- parameter$id
  testit::assert(beautier::is_id(id))
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
