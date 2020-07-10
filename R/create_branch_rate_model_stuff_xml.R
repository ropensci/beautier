#' Internal function called by \link{create_branch_rate_model_xml}
#'
#' It generates the desired XML for some circumstances.
#' Yes, that is a vague description.
#' Would be nice if someone would untangle this :-)
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_branch_rate_model_stuff_xml <- function(# nolint long function name indeed, which is fine for an internal function
  inference_model
) {
  # Do not be smart yet
  mrca_prior <- inference_model$mrca_prior
  has_non_strict_clock_model <- beautier::get_has_non_strict_clock_model(
    list(inference_model$clock_model)
  )
  has_no_mrca_prior <- beautier::is_one_na(inference_model$mrca_prior)
  has_non_strict_clock <- get_has_non_strict_clock_model(
    list(inference_model$clock_model)
  )
  has_mrca_prior <- !has_no_mrca_prior
  has_strict_clock <- !has_non_strict_clock
  testthat::expect_true(!(has_no_mrca_prior || has_non_strict_clock))
  testthat::expect_true(!has_no_mrca_prior && !has_non_strict_clock)
  testthat::expect_true(has_mrca_prior && has_strict_clock)
  has_mrca_prior_distr <- !beautier::is_one_na(mrca_prior$mrca_distr)

  if (has_mrca_prior_distr) {
    testthat::expect_true(has_mrca_prior && has_strict_clock)
    testit::assert(!beautier::is_one_na(mrca_prior$alignment_id))
    paste0(
      "<branchRateModel ",
      "id=\"StrictClock.c:", mrca_prior$alignment_id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
      "clock.rate=\"@clockRate.c:", mrca_prior$alignment_id, "\"/>" # nolint this is no absolute path
    )
  } else {
    testthat::expect_true(has_mrca_prior && has_strict_clock)
    text <- NULL
    testit::assert(!beautier::is_one_na(mrca_prior$alignment_id))
    text <- c(
      text,
      paste0(
        "<branchRateModel id=\"StrictClock.c:", mrca_prior$alignment_id, "\" ",
        "spec=\"beast.evolution.branchratemodel.StrictClockModel\">"
      )
    )
    testit::assert(!beautier::is_one_na(mrca_prior$alignment_id))
    text <- c(
      text,
      paste0(
        "    <parameter id=\"clockRate.c:", mrca_prior$alignment_id, "\" ",
        "estimate=\"false\" name=\"clock.rate\">1.0</parameter>"
      )
    )
    text <- c(text, paste0("</branchRateModel>"))
    text
  }
}
