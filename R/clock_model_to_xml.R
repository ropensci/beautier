#' Converts a clock model to the \code{prior} section of the
#' XML as text
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @author Richel J.C. Bilderbeek
clock_model_to_xml_prior <- function(
  clock_model
) {
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (is_rln_clock_model(clock_model)) {

    text <- c(text, paste0("<prior id=\"MeanRatePrior.c:", id, "\" ",
      "name=\"distribution\" x=\"@ucldMean.c:", id, "\">"))
    text <- c(text,
      indent(
        distr_to_xml(
          distr = clock_model$mean_rate_prior_distr
        ),
        n_spaces = 4
      )
    )
    text <- c(text, paste0("</prior>"))

    text <- c(text, paste0("<prior ",
      "id=\"ucldStdevPrior.c:", id, "\" name=\"distribution\" ",
      "x=\"@ucldStdev.c:", id, "\">"))
    text <- c(text,
      indent(
        distr_to_xml(
          distr = clock_model$ucldstdev_distr
        ),
        n_spaces = 4
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  text
}

#' Converts a clock model to the \code{branchRateModel} section of the
#' XML as text
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
clock_model_to_xml_brm <- function(
  clock_model
) {
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (is_strict_clock_model(clock_model)) {
    text <- c(text, paste0("<branchRateModel ",
      "id=\"StrictClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\">"))
    # initialization may happen here
    clock_model$clock_rate_param$id <- id
    text <- c(
      text,
      beautier::indent(
        beautier::parameter_to_xml(clock_model$clock_rate_param),
        n_spaces = 4
      )
    )
    text <- c(text, "</branchRateModel>")
  } else if (is_rln_clock_model(clock_model)) {
    mparam_id <- clock_model$mparam_id
    text <- c(text, paste0("<branchRateModel ",
      "id=\"RelaxedClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" ",
      "rateCategories=\"@rateCategories.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\">"))

    text <- c(text, paste0("    <LogNormal ",
      "id=\"LogNormalDistributionModel.c:", id, "\" ",
      "S=\"@ucldStdev.c:", id, "\" meanInRealSpace=\"true\" name=\"distr\">"))
    text <- c(text, paste0("        <parameter ",
      "id=\"RealParameter.", mparam_id, "\" ",
      "estimate=\"false\" lower=\"0.0\" name=\"M\" ",
      "upper=\"1.0\">1.0</parameter>"))
    text <- c(text, paste0("    </LogNormal>"))
    text <- c(text, paste0("    <parameter ",
      "id=\"ucldMean.c:", id, "\" estimate=\"false\" ",
      "name=\"clock.rate\">1.0</parameter>"))
    text <- c(text, paste0("</branchRateModel>"))
  }
  text
}

#' Converts a non-first clock model to the \code{branchRateModel}
#' section of the XML as text
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
clock_model_to_xml_brm_nonfirst <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  clock_model
) {
  id <- clock_model$id
  text <- NULL
  if (is_strict_clock_model(clock_model)) {
    text <- c(text, paste0("<branchRateModel ",
      "id=\"StrictClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
      "clock.rate=\"@clockRate.c:", id, "\"/>"))
  }
  text
}

#' Converts a clock model to the \code{state} section of the
#' XML as text
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @author Richel J.C. Bilderbeek
clock_model_to_xml_state <- function(
  clock_model
) {
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (is_rln_clock_model(clock_model)) {
    text <- c(
      text,
      paste0(
        "<parameter id=\"ucldMean.c:", id, "\" ",
        "name=\"stateNode\">", clock_model$mean_clock_rate, "</parameter>"
      )
    )
    # ucldStdev.cis always 0.1, cannot set it to other value
    text <- c(text, paste0("<parameter id=\"ucldStdev.c:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">0.1</parameter>"))
    if (clock_model$n_rate_categories > -1) {
      # value is always 1 if number of rate categories is not -1
      # no idea how to calculate the dimension
      text <- c(text, paste0("<stateNode id=\"rateCategories.c:", id, "\" ",
        "spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>"))
    }
  }

  text
}
