#' Indicator adjustment methods
#'
#' This can be used to signal no adjustment for an indicator.
#' @rdname adjustments
#' @include indicators-ferritin.R
#' @export
adjustment_ferritin_arithmetic_correction <- function() {
  with_name(ferritin_adjustment_arithmetic_correction, "arithmetic_correction")
}

#' @rdname adjustments
#' @include indicators-ferritin.R
#' @export
adjustment_ferritin_regression_correction <- function() {
  with_name(ferritin_adjustment_regression_correction, "regression_correction")
}

#' @rdname adjustments
#' @include indicators-ferritin.R
#' @export
adjustment_ferritin_rm_agp_crp <- function() {
  with_name(ferritin_adjustment_rm_agp_crp, "rm_agp_crp")
}

#' @rdname adjustments
#' @include indicators-ferritin.R
#' @export
adjustment_ferritin_cutoff <- function() {
  with_name(ferritin_adjustment_cutoff, "cutoff")
}

adjustment_name <- function(adjustment) {
  stopifnot(is_adjustment(adjustment))
  adjustment$name
}

with_name <- function(adj, name) {
  adj$name <- name
  adj
}
