# For the exported indicators we want them all to be functions and to start
# with `indicator_` to make working with them easier in code.

#' Indicators
#'
#' @include indicators-anaemia.R
#' @rdname indicators
#' @export
indicator_anaemia <- function() {
  anaemia_indicator
}

#' @param ferritin_adjustment the adjustment method for ferritin.
#' @include indicators-ferritin.R
#' @rdname indicators
#' @export
indicator_ferritin <- function(ferritin_adjustment) {
  stopifnot(is_adjustment(ferritin_adjustment))
  list(
    ferritin_indicator(no_adjustment),
    ferritin_indicator(ferritin_adjustment)
  )
}

#' @rdname indicators
#' @export
indicator_ida <- function() {
  list(
    ida_indicator_unadjusted,
    ida_indicator_adjusted
  )
}

#' @include indicators-iodine.R
#' @rdname indicators
#' @export
indicator_iodine <- function() {
  iodine_indicator
}
