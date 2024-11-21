#' @include indicators-anaemia.R
#' @include indicators-iodine.R
#' @include indicators-ferritin.R
#' @include indicators-iron-deficiency-anaemia.R
global_indicators <- list(
  iodine_indicator,
  anaemia_indicator,
  ida_indicator(no_adjustment),
  ferritin_indicator(ferritin_adjustment_cutoff),
  ferritin_indicator(no_adjustment)
)
