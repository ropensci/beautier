#' Internal function to create the \code{substModel} section
#' @inheritParams default_params_doc
#' @return the \code{substModel} section as XML text
#' @examples
#' check_empty_beautier_folder()
#'
#' # Inference model must be initialized
#' inference_model <- create_inference_model(
#'   site_model = create_jc69_site_model(id = 123)
#' )
#' create_subst_model_xml(
#'   inference_model = inference_model
#' )
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_subst_model_xml <- function(
  inference_model
) {
  site_model <- inference_model$site_model
  check_true(is_site_model(site_model))

  if (is_jc69_site_model(site_model)) {
    return(create_jc69_subst_model_xml(site_model))
  } else if (is_hky_site_model(site_model)) {
    return(create_hky_subst_model_xml(site_model))
  } else if (is_tn93_site_model(site_model)) {
    return(create_tn93_subst_model_xml(site_model))
  } else {
    check_true(is_gtr_site_model(site_model))
    return(create_gtr_subst_model_xml(site_model))
  }
}

#' Converts a JC69 site model to XML,
#' used in the \code{substModel} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_jc69_subst_model_xml <- function( # nolint indeed a long function name, which is fine for internal functions
  site_model
) {
  check_true(is_site_model(site_model))
  id <- site_model$id
  check_true(is_id(id))
  check_true(is_jc69_site_model(site_model))
  paste0("<substModel ", "id=\"JC69.s:", id, "\" spec=\"JukesCantor\"/>") # nolint this is no absolute path
}

#' Converts a site model to XML,
#'   used in the \code{substModel} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_hky_subst_model_xml <- function( # nolint indeed a long function name, which is fine for internal functions
  site_model
) {
  check_true(is_site_model(site_model))
  id <- site_model$id
  check_true(is_id(id))

  text <- NULL
  freq_equilibrium_text <- indent(
    freq_equilibrium_to_xml(site_model$freq_equilibrium, id)
  )
  check_true(is_hky_site_model(site_model))
  text <- c(
    text,
    paste0(
      "<substModel ",
      "id=\"hky.s:", id, "\" spec=\"HKY\" kappa=\"@kappa.s:", id, "\">"
    )
  )
  text <- c(text, freq_equilibrium_text)
  text <- c(text, paste0("</substModel>"))
  text
}

#' Converts a TN93 site model to XML,
#'   used in the \code{substModel} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_tn93_subst_model_xml <- function( # nolint indeed a long function name, which is fine for internal functions
  site_model
) {
  check_true(is_site_model(site_model))
  id <- site_model$id
  check_true(is_id(id))

  text <- NULL
  freq_equilibrium_text <- indent(
    freq_equilibrium_to_xml(site_model$freq_equilibrium, id)
  )

  check_true(is_tn93_site_model(site_model))
  subst_model_line <- paste0(
    "<substModel id=\"tn93.s:", id, "\" spec=\"TN93\""
  )
  if (site_model$kappa_1_param$estimate == TRUE) {
    subst_model_line <- paste0(
      subst_model_line, " kappa1=\"@kappa1.s:", id, "\" "
    )
  }
  if (site_model$kappa_2_param$estimate == TRUE) {
    subst_model_line <- paste0(
      subst_model_line, "kappa2=\"@kappa2.s:", id, "\""
    )
  }
  subst_model_line <- paste0(subst_model_line, ">")
  text <- c(text, subst_model_line)
  if (site_model$kappa_1_param$estimate == FALSE) {
    text <- c(
      text,
      paste0(
        "<parameter id=\"kappa1.s:", id, "\" ",
        "estimate=\"false\" ",
        "lower=\"", site_model$kappa_1_param$lower, "\" ",
        "name=\"kappa1\">", site_model$kappa_1_param$value, "</parameter>"
      )
    )
  }
  if (site_model$kappa_2_param$estimate == FALSE) {
    text <- c(
      text,
      paste0(
        "<parameter id=\"kappa2.s:", id, "\" ",
        "estimate=\"false\" ",
        "lower=\"", site_model$kappa_2_param$lower, "\" ",
        "name=\"kappa2\">", site_model$kappa_2_param$value, "</parameter>"
      )
    )
  }
  text <- c(text, freq_equilibrium_text)
  text <- c(text, paste0("</substModel>"))
  text
}

#' Converts a GTR site model to XML,
#'   used in the \code{substModel} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_gtr_subst_model_xml <- function( # nolint indeed a long function name, which is fine for internal functions
  site_model
) {
  check_true(is_site_model(site_model))
  id <- site_model$id
  check_true(is_id(id))

  text <- NULL
  freq_equilibrium_text <- indent(
    freq_equilibrium_to_xml(site_model$freq_equilibrium, id)
  )

  check_true(is_gtr_site_model(site_model))

  subst_model_xml <- paste0(
    "<substModel ", "id=\"gtr.s:", id, "\" spec=\"GTR\""
  )
  if (site_model$rate_ac_param$estimate == TRUE) {
    subst_model_xml <- paste0(
      subst_model_xml, " rateAC=\"@rateAC.s:", id, "\""
    )
  }
  if (site_model$rate_ag_param$estimate == TRUE) {
    subst_model_xml <- paste0(
      subst_model_xml, " rateAG=\"@rateAG.s:", id, "\""
    )
  }
  if (site_model$rate_at_param$estimate == TRUE) {
    subst_model_xml <- paste0(
      subst_model_xml, " rateAT=\"@rateAT.s:", id, "\""
    )
  }
  if (site_model$rate_cg_param$estimate == TRUE) {
    subst_model_xml <- paste0(
      subst_model_xml, " rateCG=\"@rateCG.s:", id, "\""
    )
  }
  if (site_model$rate_gt_param$estimate == TRUE) {
    subst_model_xml <- paste0(
      subst_model_xml, " rateGT=\"@rateGT.s:", id, "\""
    )
  }
  subst_model_xml <- paste0(subst_model_xml, ">")
  text <- c(text, subst_model_xml)

  if (site_model$rate_ac_param$estimate == FALSE) {
    site_model$rate_ac_param$id <- id
    text <- c(
      text,
      indent(
        parameter_to_xml_rate_ac(
          site_model$rate_ac_param, which_name = "rate_name"
        )
      )
    )
  }


  if (site_model$rate_ag_param$estimate == FALSE) {
    site_model$rate_ag_param$id <- id
    text <- c(
      text,
      indent(
        parameter_to_xml_rate_ag(
          site_model$rate_ag_param, which_name = "rate_name"
        )
      )
    )
  }


  if (site_model$rate_at_param$estimate == FALSE) {
    site_model$rate_at_param$id <- id
    text <- c(
      text,
      indent(
        parameter_to_xml_rate_at(
          site_model$rate_at_param, which_name = "rate_name"
        )
      )
    )
  }


  if (site_model$rate_cg_param$estimate == FALSE) {
    site_model$rate_cg_param$id <- id
    text <- c(
      text,
      indent(
        parameter_to_xml_rate_cg(
          site_model$rate_cg_param, which_name = "rate_name"
        )
      )
    )
  }

  if (site_model$rate_ct_param$estimate == FALSE) {
    site_model$rate_ct_param$id <- id
    text <- c(
      text,
      indent(
        parameter_to_xml_rate_ct(
          site_model$rate_ct_param, which_name = "rate_name"
        )
      )
    )
  }
  if (site_model$rate_gt_param$estimate == FALSE) {
    site_model$rate_gt_param$id <- id
    text <- c(
      text,
      indent(
        parameter_to_xml_rate_gt(
          site_model$rate_gt_param,
          which_name = "rate_name"
        )
      )
    )
  }
  text <- c(text, freq_equilibrium_text)
  text <- c(text, paste0("</substModel>"))
  text
}
