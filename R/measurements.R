measurement_units <- c(
  "mcmol_l",
  "g_l",
  "mcg_l",
  "ng_ml",
  "nmol_l",
  "mg_l",
  "m",
  "ft"
)

new_measurement <- function(x, unit) {
  stopifnot(is.double(x), is.character(unit), unit %in% measurement_units)
  new_vctr(x, unit = unit, class = "vctrs_measurement")
}

measurement_mcmol_l <- function(x = numeric()) {
  new_measurement(vec_cast(x, double()), "mcmol_l")
}

measurement_mcg_l <- function(x = numeric()) {
  new_measurement(vec_cast(x, double()), "mcg_l")
}

measurement_g_l <- function(x = numeric()) {
  new_measurement(vec_cast(x, double()), "g_l")
}

measurement_ng_ml <- function(x) {
  new_measurement(vec_cast(x, double()), "ng_ml")
}

measurement_nmol_l <- function(x) {
  new_measurement(vec_cast(x, double()), "nmol_l")
}

measurement_mg_l <- function(x) {
  new_measurement(vec_cast(x, double()), "mg_l")
}

measurement_meters <- function(x) {
  new_measurement(vec_cast(x, double()), "m")
}

measurement_feet <- function(x) {
  new_measurement(vec_cast(x, double()), "ft")
}

is_measurement <- function(x) {
  inherits(x, "vctrs_measurement")
}

measurement_unit <- function(x) {
  stopifnot(is_measurement(x))
  attr(x, "unit", exact = TRUE)
}

#' @export
format.vctrs_measurement <- function(x, ...) {
  paste(as.character(x), attr(x, "unit"))
}

is_measurement_mcmol_l <- function(x) {
  is_measurement(x) && measurement_unit(x) == "mcmol_l"
}
is_measurement_mcg_l <- function(x) {
  is_measurement(x) && measurement_unit(x) == "mcg_l"
}
is_measurement_ng_ml <- function(x) {
  is_measurement(x) && measurement_unit(x) == "ng_ml"
}
is_measurement_nmol_l <- function(x) {
  is_measurement(x) && measurement_unit(x) == "nmol_l"
}
is_measurement_g_l <- function(x) {
  is_measurement(x) && measurement_unit(x) == "g_l"
}
is_measurement_mg_l <- function(x) {
  is_measurement(x) && measurement_unit(x) == "mg_l"
}

#' @export
vec_ptype_abbr.vctrs_measurement <- \(x, ...) measurement_unit(x)

#' @export
vec_proxy_equal.vctrs_measurement <- \(x, ...) vec_data(x)

#' @export
vec_proxy_compare.vctrs_measurement <- \(x, ...) vec_data(x)

#' @export
vec_proxy_order.vctrs_measurement <- \(x, ...) vec_data(x)

#' @export
vec_ptype2.double.vctrs_measurement <- \(x, y, ...) double()

#' @export
vec_ptype2.vctrs_measurement.double <- \(x, y, ...) double()

#' @export
vec_ptype2.vctrs_measurement.character <- \(x, y, ...) character()

#' @export
vec_cast.character.vctrs_measurement <- \(x, to, ...) as.character(vec_data(x))

#' @export
vec_cast.double.vctrs_measurement <- \(x, to, ...) vec_data(x)

#' @method vec_arith vctrs_measurement
#' @export
vec_arith.vctrs_measurement <- function(op, x, y, ...) {
  UseMethod("vec_arith.vctrs_measurement", y)
}

#' @method vec_arith.vctrs_measurement MISSING
#' @import vctrs
#' @export
vec_arith.vctrs_measurement.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    `-` = new_measurement(vec_data(x) * -1, measurement_unit(x)),
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.vctrs_measurement default
#' @export
vec_arith.vctrs_measurement.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
