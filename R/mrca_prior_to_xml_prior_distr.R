#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#'
#' These lines start with '<distribution id='
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @param taxa_names_with_ids taxa names that already have received
#'   an ID. Causes the XML to \code{idref} these
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' @noRd
mrca_prior_to_xml_prior_distr <- function( # nolint beautier function
  mrca_prior,
  has_non_strict_clock_model = FALSE,
  taxa_names_with_ids = NULL
) {
  testit::assert(is_mrca_prior(mrca_prior)) # nolint beautier function
  text <- NULL
  if (!has_non_strict_clock_model && # nolint beautier function
      !is_one_na(mrca_prior$mrca_distr) # nolint beautier function
  ) {
    testit::assert(!is_one_na(mrca_prior$alignment_id)) # nolint beautier function
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
      indent( # nolint beautier function
        distr_to_xml(create_uniform_distr( # nolint beautier function
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
  testit::assert(!is_one_na(mrca_prior$alignment_id)) # nolint beautier function
  opening_tag <- paste0(
    opening_tag, "tree=\"@Tree.t:", mrca_prior$alignment_id, "\">"
  )

  text <- c(text, opening_tag)
  text <- c(
    text,
    indent( # nolint beautier function
      mrca_prior_to_xml_taxonset( # nolint beautier function
        mrca_prior,
        taxa_names_with_ids
      ),
      n_spaces = 4
    )
  )
  if (is_distr(mrca_prior$mrca_distr)) { # nolint beautier function
    text <- c(text, indent(distr_to_xml(mrca_prior$mrca_distr), n_spaces = 4)) # nolint beautier function
  }
  text <- c(text, paste0("</distribution>"))


  text
}
