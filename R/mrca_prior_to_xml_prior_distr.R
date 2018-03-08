#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<distribution id='
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
mrca_prior_to_xml_prior_distr <- function( # nolint internal function
  mrca_prior,
  has_non_strict_clock_model = FALSE
) {
  testit::assert(is_mrca_prior(mrca_prior))
  text <- NULL
  if (mrca_prior$is_monophyletic) {
    text <- c(
      text,
      paste0(
        "<prior id=\"ClockPrior.c:", mrca_prior$alignment_id, "\" ",
        "name=\"distribution\" ",
        "x=\"@clockRate.c:", mrca_prior$alignment_id, "\">"
      )
    )
    text <- c(
      text,
      indent(
        distr_to_xml(create_uniform_distr(
          id = mrca_prior$clock_prior_distr_id)
        ),
        n_spaces = 4
      )
    )
    text <- c(text, paste0("</prior>"))
  }

  opening_tag <- paste0(
    "<distribution id=\"", mrca_prior$name, ".prior\" ",
    "spec=\"beast.math.distributions.MRCAPrior\" "
  )
  if (mrca_prior$is_monophyletic) {
    opening_tag <- paste0(opening_tag, "monophyletic=\"true\" ")
  }
  opening_tag <- paste0(
    opening_tag, "tree=\"@Tree.t:", mrca_prior$alignment_id, "\">"
  )

  text <- c(text, opening_tag)
  text <- c(text, indent(mrca_prior_to_xml_taxonset(mrca_prior), n_spaces = 4)) # nolint internal function
  if (is_distr(mrca_prior$mrca_distr)) {
    text <- c(text, indent(distr_to_xml(mrca_prior$mrca_distr), n_spaces = 4))
  }
  text <- c(text, paste0("</distribution>"))


  text
}
