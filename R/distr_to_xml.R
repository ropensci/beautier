#' Internal function
#'
#' Converts a distribution to XML
#' @inheritParams default_params_doc
#' @param distr a distribution,
#'   as created by \code{\link{create_distr}})
#' @return the distribution as XML text
#' @examples
#' check_empty_beautier_folder()
#'
#' distr_to_xml(create_uniform_distr(id = 1))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
distr_to_xml <- function(
  distr,
  beauti_options = create_beauti_options()
) {
  beautier::check_beauti_options(beauti_options)
  text <- NULL
  id <- distr$id
  if (!beautier::is_id(id)) {
    stop("distribution must have an ID")
  }
  if (beautier::is_beta_distr(distr)) {
    text <- c(text, beautier::distr_to_xml_beta(distr = distr, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_exp_distr(distr)) {
    text <- c(text, beautier::distr_to_xml_exp(distr = distr, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_gamma_distr(distr)) {
    text <- c(text, beautier::gamma_distr_to_xml(gamma_distr = distr, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_inv_gamma_distr(distr)) {
    text <- c(text, beautier::distr_to_xml_inv_gamma(distr = distr, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_laplace_distr(distr)) {
    text <- c(text, beautier::distr_to_xml_laplace(distr = distr, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_log_normal_distr(distr)) {
    text <- c(text, beautier::distr_to_xml_log_normal(distr = distr, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_normal_distr(distr)) {
    text <- c(text, beautier::distr_to_xml_normal(distr = distr, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_one_div_x_distr(distr)) {
    text <- c(text, beautier::distr_to_xml_one_div_x(distr = distr, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (beautier::is_poisson_distr(distr)) {
    text <- c(text, beautier::distr_to_xml_poisson(distr = distr, beauti_options = beauti_options)) # nolint indeed a long line
  } else {
    testit::assert(beautier::is_uniform_distr(distr))
    text <- c(text, beautier::distr_to_xml_uniform(distr = distr, beauti_options = beauti_options)) # nolint indeed a long line
  }
  testit::assert(beautier::is_xml(text))
  text
}

#' Internal function
#'
#' Converts a beta distribution to XML
#' @inheritParams default_params_doc
#' @param distr a beta distribution,
#'   as created by \code{\link{create_beta_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
distr_to_xml_beta <- function(
  distr,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_beta_distr(distr))
  beautier::check_beauti_options(beauti_options)
  id <- distr$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<Beta id=\"Beta.", id, "\" name=\"distr\">"))
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$alpha)
    )
  )
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$beta)
    )
  )
  text <- c(text, paste0("</Beta>"))
  text
}

#' Internal function
#'
#' Converts an exponential distribution to XML
#' @inheritParams default_params_doc
#' @param distr an exponential distribution,
#'   as created by \code{\link{create_exp_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
distr_to_xml_exp <- function(
  distr,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_exp_distr(distr))
  beautier::check_beauti_options(beauti_options)
  id <- distr$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<Exponential ",
    "id=\"Exponential.", id, "\" name=\"distr\">"))
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$mean)
    )
  )
  text <- c(text, paste0("</Exponential>"))
  text
}

#' Internal function
#'
#' Converts an inverse-gamma distribution to XML
#' @inheritParams default_params_doc
#' @param distr an inverse-gamma distribution,
#'   as created by \code{\link{create_inv_gamma_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
distr_to_xml_inv_gamma <- function(
  distr,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_inv_gamma_distr(distr))
  beautier::check_beauti_options(beauti_options)
  id <- distr$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<InverseGamma ",
    "id=\"InverseGamma.", id, "\" name=\"distr\">"))
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$alpha)
    )
  )
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$beta)
    )
  )
  text <- c(text, paste0("</InverseGamma>"))
  text
}

#' Internal function
#'
#' Converts a Laplace distribution to XML
#' @inheritParams default_params_doc
#' @param distr a Laplace distribution
#'   as created by \code{\link{create_laplace_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
distr_to_xml_laplace <- function(
  distr,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_laplace_distr(distr))
  beautier::check_beauti_options(beauti_options)
  id <- distr$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<LaplaceDistribution ",
    "id=\"LaplaceDistribution.", id, "\" name=\"distr\">"))
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$mu)
    )
  )
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$scale)
    )
  )
  text <- c(text, paste0("</LaplaceDistribution>"))
  text
}

#' Internal function
#'
#' Converts a log-normal distribution to XML
#' @inheritParams default_params_doc
#' @param distr a log-normal distribution,
#'   as created by \code{\link{create_log_normal_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
distr_to_xml_log_normal <- function(
  distr,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_log_normal_distr(distr))
  beautier::check_beauti_options(beauti_options)
  id <- distr$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<LogNormal ",
    "id=\"LogNormalDistributionModel.", id, "\" name=\"distr\">"))
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$m)
    )
  )
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$s)
    )
  )
  text <- c(text, paste0("</LogNormal>"))
  text
}


#' Internal function
#'
#' Converts a normal distribution to XML
#' @inheritParams default_params_doc
#' @param distr a normal distribution,
#'   as created by \code{\link{create_normal_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
distr_to_xml_normal <- function(
  distr,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_normal_distr(distr))
  beautier::check_beauti_options(beauti_options)
  id <- distr$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<Normal ",
    "id=\"Normal.", id, "\" name=\"distr\">"))
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$mean)
    )
  )
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$sigma)
    )
  )
  text <- c(text, paste0("</Normal>"))
  text
}

#' Internal function
#'
#' Converts a 1/x distribution to XML
#' @inheritParams default_params_doc
#' @param distr a 1/x distribution,
#'   as created by \code{\link{create_one_div_x_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
distr_to_xml_one_div_x <- function(
  distr,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_one_div_x_distr(distr))
  beautier::check_beauti_options(beauti_options)
  id <- distr$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<OneOnX ",
    "id=\"OneOnX.", id, "\" name=\"distr\"/>")) # nolint this is no absolute path
  text
}

#' Internal function
#'
#' Converts a Poisson distribution to XML
#' @inheritParams default_params_doc
#' @param distr a Poisson distribution,
#'   as created by \code{\link{create_poisson_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
distr_to_xml_poisson <- function(
  distr,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_poisson_distr(distr))
  beautier::check_beauti_options(beauti_options)
  id <- distr$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<distr ",
    "id=\"Poisson.", id, "\" ",
    "spec=\"beast.math.distributions.Poisson\">"))
  text <- c(text,
    beautier::indent(
      beautier::parameter_to_xml(distr$lambda),
    )
  )
  text <- c(text, paste0("</distr>"))
  text
}

#' Internal function
#'
#' Converts a uniform distribution to XML
#' @inheritParams default_params_doc
#' @param distr a uniform distribution,
#'   as created by \code{\link{create_uniform_distr}})
#' @return the distribution as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
distr_to_xml_uniform <- function(
  distr,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_uniform_distr(distr))
  beautier::check_beauti_options(beauti_options)
  id <- distr$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  line_begin <- paste0("<Uniform id=\"Uniform.", id, "\" name=\"distr\"")
  line_end <- "/>" # nolint this is no absolute path
  upper <- distr$upper
  if (beautier::is_one_na(upper)) {
    text <- c(text, paste0(line_begin, line_end))
  } else if (is.infinite(upper)) {
    text <- c(text, paste0(line_begin, " upper=\"Infinity\"", line_end))
  } else {
    text <- c(text, paste0(line_begin, " upper=\"", upper, "\"", line_end))
  }
  text
}
