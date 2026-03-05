is_iodine_implausible <- function(iodine) {
  # iodine <- vec_cast(iodine, double())
  # iodine < 0 | iodine > 1000

  # git427
  vals <- rep(FALSE, length(iodine))
  vals[is.na(iodine)] <- NA
  vals
}

iodine_is_implausible <- function(x, ...) {
  UseMethod("iodine_is_implausible")
}

#' @export
iodine_is_implausible.default <- function(x, ...) {
  iodine_implausible_values(x)
}

iodine_adjustment <- function(value, age, sex) {
  value
}

iodine_implausible_values <- function(value, ...) {
  na <- measurement_mcg_l(NA_real_)

  value[is_iodine_implausible(value)] <- na
  value
}

iodine_indicator <- indicator(
  name = "Urinary Iodine Concentration in \u00B5g/L",
  abbreviated_name = "iodine",
  value_concept = "iodine",
  export_value_name = "iodine",
  required_concepts = c(
    "age",
    "pregnancy_status"
  ),
  global_condition = age_in_years(age) >= 0, # no restrictions
  categories = list(),
  adjustment = adjustment(
    required_concepts = c(
      "age",
      "sex"
    ),
    fun = iodine_adjustment
  ),
  implausible_values = adjustment(
    required_concepts = c(
      "age",
      "sex"
    ),
    fun = iodine_implausible_values
  ),
  prev_value_cutoffs = lapply(c(300, 200, 150, 100, 50, 20), function(x) {
    new_prev_cutoff(
      new_function(pairlist2(value = ), bquote(value < .(x))),
      paste0("iodine_p", x)
    )
  }),
  prevalence_categories = list(),
  prevalence_category_names = c(),
  drop_columns = list(
    short = c(
      #"iodine_mean",
      "iodine_mean_sd",
      "iodine_mean_ll",
      "iodine_mean_ul"
    ),
    long = NULL
  ),
  rename_columns = list(
    short = c("iodine_median_r" = "iodine_50percentile"), # left is new name, right is old name
    long = NULL
  ),
  reorder_columns = list(
    short = c("iodine_median_r" = "iodine_25percentile"), # left goes right before right
    long = NULL
  ),
  prevalence_reports = list(
    long = FALSE,
    short = TRUE
  )
)
