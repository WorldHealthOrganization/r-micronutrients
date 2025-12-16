#' @rdname adjustments
#' @include indicators-ferritin.R
#' @export
adjustment_ferritin_arithmetic_correction <- function() {
  ferritin_adjustment_arithmetic_correction
}

#' @rdname adjustments
#' @include indicators-ferritin.R
#' @export
adjustment_ferritin_regression_correction <- function() {
  ferritin_adjustment_regression_correction
}

#' @rdname adjustments
#' @include indicators-ferritin.R
#' @export
adjustment_ferritin_rm_agp_crp <- function() {
  ferritin_adjustment_rm_agp_crp
}

#' @rdname adjustments
#' @include indicators-ferritin.R
#' @export
adjustment_ferritin_cutoff <- function() {
  ferritin_adjustment_cutoff
}


#' Indicator adjustment methods
#'
#' This can be used to signal no adjustment for an indicator.
#' @rdname adjustments
#' @export
adjustment_none <- function() {
  no_adjustment
}
