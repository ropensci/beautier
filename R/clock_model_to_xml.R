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
    n_discrete_rates <- clock_model$n_rate_categories
    mparam_id <- clock_model$mparam_id
    line <- paste0("<branchRateModel ",
      "id=\"RelaxedClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" ",
      "clock.rate=\"@ucldMean.c:", id, "\" ",
      ifelse(clock_model$normalize_mean_clock_rate == TRUE,
        "normalize=\"true\" ", ""),
      "numberOfDiscreteRates=\"",
      n_discrete_rates,
      "\" ",
      "rateCategories=\"@rateCategories.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\">"
    )

    text <- c(text, line)
    text <- c(text, paste0("    <LogNormal ",
      "id=\"LogNormalDistributionModel.c:", id, "\" ",
      "S=\"@ucldStdev.c:", id, "\" meanInRealSpace=\"true\" name=\"distr\">"))
    text <- c(text, paste0("        <parameter ",
      "id=\"RealParameter.", mparam_id, "\" ",
      "estimate=\"false\" lower=\"0.0\" name=\"M\" ",
      "upper=\"1.0\">1.0</parameter>"))
    text <- c(text, paste0("    </LogNormal>"))
    # TODO: move elsewhere
    if (1 == 2) {
      text <- c(text, paste0("    <parameter ",
        "id=\"ucldMean.c:", id, "\" estimate=\"false\" ",
        "name=\"clock.rate\">1.0</parameter>"))
    }
    text <- c(text, paste0("</branchRateModel>"))
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
