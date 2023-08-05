#' Get the default `namespace` element value of the `beast` XML tag.
#'
#' Get the default `namespace` element value of the `beast` XML tag.
#' @return the default `namespace` element value of the `beast` XML tag.
#' @seealso
#'   * For BEAST2 v2.4, use \link{get_default_beast_namespace_v2_4}
#'   * For BEAST2 v2.6, use \link{get_default_beast_namespace_v2_6}
#' @examples
#' get_default_beast_namespace()
#' @author Richèl J.C. Bilderbeek
#' @export
get_default_beast_namespace <- function() {
  get_default_beast_namespace_v2_4()
}

#' Get the default `namespace` element value of the `beast` XML tag
#' for BEAST 2.4
#'
#' Get the default `namespace` element value of the `beast` XML tag
#' for BEAST 2.4
#' @return the default `namespace` element value of the `beast` XML tag
#' for BEAST 2.4
#' @examples
#' get_default_beast_namespace_v2_4()
#' @author Richèl J.C. Bilderbeek
#' @export
get_default_beast_namespace_v2_4 <- function() { # nolint indeed a long function name
  "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood" # nolint indeed a long line
}

#' Get the default `namespace` element value of the `beast` XML tag
#' for BEAST 2.6
#'
#' Get the default `namespace` element value of the `beast` XML tag
#' for BEAST 2.6
#' @return the default `namespace` element value of the `beast` XML tag
#' for BEAST 2.6
#' @examples
#' get_default_beast_namespace_v2_6()
#' @author Richèl J.C. Bilderbeek
#' @export
get_default_beast_namespace_v2_6 <- function() { # nolint indeed a long function name
  "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.base.evolution.alignment:beast.pkgmgmt:beast.base.core:beast.base.inference:beast.base.evolution.tree.coalescent:beast.pkgmgmt:beast.base.core:beast.base.inference.util:beast.evolution.nuc:beast.base.evolution.operator:beast.base.inference.operator:beast.base.evolution.sitemodel:beast.base.evolution.substitutionmodel:beast.base.evolution.likelihood" # nolint indeed a long line
}
