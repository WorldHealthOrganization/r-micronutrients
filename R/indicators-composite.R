composite_indicator <- function(
  name,
  abbreviated_name,
  compute_function,
  prevalence_categories,
  requires_indicators
) {
  structure(
    list(
      name = name,
      compute_function = compute_function,
      abbreviated_name = abbreviated_name,
      prevalence_categories = prevalence_categories,
      required_indicators = requires_indicators
    ),
    class = "composite_indicator"
  )
}

is_composite_indicator <- function(x) {
  inherits(x, "composite_indicator")
}

#' @exportS3Method
indicator_name.composite_indicator <- function(indicator) {
  indicator$name
}

#' @exportS3Method
indicator_value_concept.composite_indicator <- function(x) {
  NULL
}

#' @exportS3Method
indicator_abbreviated_name.composite_indicator <- function(indicator) {
  indicator$abbreviated_name
}

#' @exportS3Method
prevalence_report_short.composite_indicator <- function(indicator) {
  TRUE
}

#' @exportS3Method
indicator_prevalence_categories.composite_indicator <- function(indicator) {
  indicator$prevalence_categories
}

#' @exportS3Method
indicator_agg_prevalence_categories.composite_indicator <- function(indicator) {
  NULL
}

#' @exportS3Method
indicator_rename_columns.composite_indicator <- function(
  indicator,
  report_type
) {
  list(
    short = NULL,
    long = NULL
  )
}

#' @exportS3Method
indicator_reorder_columns.composite_indicator <- function(
  indicator,
  report_type
) {
  list(
    short = NULL,
    long = NULL
  )
}

#' @exportS3Method
indicator_drop_columns.composite_indicator <- function(indicator, report_type) {
  NULL
}

#' @exportS3Method
indicator_export_value_name.composite_indicator <- function(indicator) {
  indicator$abbreviated_name
}


#' @exportS3Method
indicator_adjustment.composite_indicator <- function(indicator) {
  no_adjustment
}

#' @export
format.composite_indicator <- function(x, ...) {
  x$name
}

#' @export
print.composite_indicator <- function(x, ...) {
  cat(format(x), "\n")
}
