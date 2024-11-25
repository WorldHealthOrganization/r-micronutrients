# For the exported indicators we want them all to be functions and to start
# with `indicator_` to make working with them easier in code.

#' Indicators
#'
#' @include indicators-anaemia.R
#' @rdname indicators
#' @export
indicator_anaemia <- function() anaemia_indicator

#' @param ferritin_adjustment the adjustment method for ferritin.
#' @include indicators-ferritin.R
#' @rdname indicators
#' @export
indicator_ferritin <- function(ferritin_adjustment = adjustment_none()) {
  ferritin_indicator(ferritin_adjustment)
}

#' @param ferritin_adjustment the adjustment method for ferritin.
#' @rdname indicators
#' @include indicators-iron-deficiency-anaemia.R
#' @export
indicator_ida <- function(ferritin_adjustment = adjustment_none()) {
  ida_indicator(ferritin_adjustment)
}

#' @include indicators-iodine.R
#' @rdname indicators
#' @export
indicator_iodine <- function() iodine_indicator
