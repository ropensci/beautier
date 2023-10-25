#' Internal function
#'
#' Converts a GTR site model to XML,
#'   used in the \code{prior} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @examples
#' gtr_site_model_to_xml_prior_distr(
#'   site_model = create_gtr_site_model(
#'     id = 1,
#'     rate_ac_prior_distr = create_uniform_distr(id = 2),
#'     rate_ag_prior_distr = create_uniform_distr(id = 3),
#'     rate_at_prior_distr = create_uniform_distr(id = 4),
#'     rate_cg_prior_distr = create_uniform_distr(id = 5),
#'     rate_gt_prior_distr = create_uniform_distr(id = 6)
#'   ),
#'   beauti_options = create_beauti_options()
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
gtr_site_model_to_xml_prior_distr <- function( # nolint indeed a long internal function name
  site_model,
  beauti_options
) {
  check_true(is_gtr_site_model(site_model))
  id <- site_model$id
  check_true(is_id(id))

  text <- NULL
  if (site_model$rate_ac_param$estimate == TRUE) {
    text <- c(
      text,
      paste0(
        "<prior id=\"RateACPrior.s:", id, "\" ",
        "name=\"distribution\" x=\"@rateAC.s:", id, "\">"
      )
    )
    text <- c(
      text,
      indent(
        distr_to_xml(
          site_model$rate_ac_prior_distr,
          beauti_options = beauti_options
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  if (site_model$rate_ag_param$estimate == TRUE) {
    text <- c(
      text,
      paste0(
        "<prior id=\"RateAGPrior.s:", id, "\" ",
        "name=\"distribution\" x=\"@rateAG.s:", id, "\">"
      )
    )
    text <- c(
      text,
      indent(
        distr_to_xml(
          site_model$rate_ag_prior_distr,
          beauti_options = beauti_options
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  if (site_model$rate_at_param$estimate == TRUE) {
    text <- c(
      text,
      paste0(
        "<prior id=\"RateATPrior.s:", id, "\" ",
        "name=\"distribution\" x=\"@rateAT.s:", id, "\">"
      )
    )
    text <- c(
      text,
      indent(
        distr_to_xml(
          site_model$rate_at_prior_distr,
          beauti_options = beauti_options
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  if (site_model$rate_cg_param$estimate == TRUE) {
    text <- c(
      text,
      paste0(
        "<prior id=\"RateCGPrior.s:", id, "\" ",
        "name=\"distribution\" x=\"@rateCG.s:", id, "\">"
      )
    )
    text <- c(
      text,
      indent(
        distr_to_xml(
          site_model$rate_cg_prior_distr,
          beauti_options = beauti_options
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  if (site_model$rate_gt_param$estimate == TRUE) {
    text <- c(
      text,
      paste0(
        "<prior id=\"RateGTPrior.s:", id, "\" ",
        "name=\"distribution\" x=\"@rateGT.s:", id, "\">"
      )
    )
    text <- c(
      text,
      indent(
        distr_to_xml(
          site_model$rate_gt_prior_distr,
          beauti_options = beauti_options
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  }

  text
}
