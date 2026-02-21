#' Create an `age` object to represent as specific units
#'
#' @param x a numeric vector of all age values
#' @param unit the unit of the values. Can be 'years', 'months' or 'days'
#'.
#' @note
#' The `age` representation is currently only usable with the individual
#' classification and the statistics. It is not meant to be used outside
#' of these functions.
#'
#' @examples
#' age(1:5, unit = "months")
#'
#' @export
age <- function(x = numeric(), unit = c("years", "months", "days")) {
  if (!is.numeric(x)) {
    stop("`x` needs to be a numeric vector")
  }
  unit <- match.arg(unit)
  if (unit == "years") {
    x <- x * one_year_in_days
  }
  if (unit == "months") {
    x <- x * one_month_in_days
  }
  new_vctr(x, class = "mn_age")
}

is_age <- function(x) {
  inherits(x, "mn_age")
}

one_month_in_days <- 30.4375
one_year_in_days <- 365.25

age_in_days <- function(x) {
  stopifnot(is_age(x))
  vec_data(x)
}

age_in_months <- function(x) {
  stopifnot(is_age(x))
  vec_data(x) / one_month_in_days
}

age_in_years <- function(x) {
  stopifnot(is_age(x))
  vec_data(x) / one_year_in_days
}

age_as_numeric <- function(x, unit) {
  if (unit == "years") {
    age_in_years(x)
  } else if (unit == "months") {
    age_in_months(x)
  } else {
    stopifnot(is_age(x))
    vec_data(x)
  }
}
