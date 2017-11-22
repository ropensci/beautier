#' Converts a clock model to the \code{<prior>} section of the
#' XML as text
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @author Richel J.C. Bilderbeek
clock_model_to_prior_xml <- function(
  id,
  clock_model
) {
  testit::assert(beautier::is_clock_model(clock_model))

  text <- NULL
  if (is_rln_clock_model(clock_model)) {
    text <- c(text, paste0("<prior ",
      "id=\"ucldStdevPrior.c:", id, "\" name=\"distribution\" ",
      "x=\"@ucldStdev.c:", id, "\">"))
    text <- c(text,
      indent(
        distr_to_xml(
          distr = get_rln_ucldstdev_distr(clock_model)
        ),
        n_spaces = 4
      )
    )
    text <- c(text, paste0("</prior>"))
  }
  text
}

#' Converts a clock model to the \code{<branchRateModel>} section of the
#' XML as text
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
clock_model_to_brm_xml <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  clock_model
) {
  text <- NULL
  if (is_strict_clock_model(clock_model)) {
    text <- c(text, paste0("<branchRateModel ",
      "id=\"StrictClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\">"))
    # initialization may happen here
    clock_model$clock_rate_parameter$id <- id
    text <- c(
      text,
      beautier::indent(
        beautier::parameter_to_xml(clock_model$clock_rate_parameter),
        n_spaces = 4
      )
    )
    text <- c(text, "</branchRateModel>")
  } else if (is_rln_clock_model(clock_model)) {
    m_parameter_id <- clock_model$m_parameter_id
    text <- c(text, paste0("<branchRateModel ",
      "id=\"RelaxedClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" ",
      "rateCategories=\"@rateCategories.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\">"))

    text <- c(text, paste0("    <LogNormal ",
      "id=\"LogNormalDistributionModel.c:", id, "\" ",
      "S=\"@ucldStdev.c:", id, "\" meanInRealSpace=\"true\" name=\"distr\">"))
    text <- c(text, paste0("        <parameter ",
      "id=\"RealParameter.", m_parameter_id, "\" estimate=\"false\" lower=\"0.0\" name=\"M\" ",
      "upper=\"1.0\">1.0</parameter>"))
    text <- c(text, paste0("    </LogNormal>"))
    text <- c(text, paste0("    <parameter ",
      "id=\"ucldMean.c:", id, "\" estimate=\"false\" ",
      "name=\"clock.rate\">1.0</parameter>"))
    text <- c(text, paste0("</branchRateModel>"))
  }
  text
}

#' Creates the second or later clock models' section in the distribution section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
clock_model_to_other_brm_xml <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  clock_model
) {
  text <- NULL
  if (is_strict_clock_model(clock_model)) {
    text <- c(text, paste0("<branchRateModel ",
      "id=\"StrictClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
      "clock.rate=\"@clockRate.c:", id, "\"/>"))
  }
  text
}
