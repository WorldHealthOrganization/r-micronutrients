# anaemia_prevalence_categories <- c(
#     mildanaemia = "Mild anaemia",
#     moderateanaemia = "Moderate anaemia",
#     severeanaemia = "Severe anaemia",
#     totalanaemia = "Total anaemia"
# )



anaemia_adjustment <- function(value, altitude, is_smoker, smokes_cigarettes_per_day) {
  value <- vec_cast(value, double())
  stopifnot(
    is.numeric(altitude),
    is.numeric(smokes_cigarettes_per_day),
    is.logical(is_smoker),
    length(value) == length(altitude),
    length(altitude) == length(is_smoker),
    length(is_smoker) == length(smokes_cigarettes_per_day)
  )

  altitude <- as.numeric(altitude)
  altitude[is.na(altitude)] <- 0
  altitude_adjustments <- (0.0056384 * altitude) + (0.0000003 * altitude^2)
  altitude_adjustments <- altitude_adjustments * -1


  smokes_cigarettes_per_day[smokes_cigarettes_per_day == 0] <- NA_real_
  smoke_adjustments <- rep.int(0, length(value))
  not_na <- !is.na(is_smoker) & !is.na(smokes_cigarettes_per_day)

  smoke_adjustments[not_na & is_smoker] <-
    (0.4565 * smokes_cigarettes_per_day[not_na & is_smoker]) +
    (-0.0078 * smokes_cigarettes_per_day[not_na & is_smoker]^2)

  smoke_adjustments[!is.na(is_smoker) & is_smoker & is.na(smokes_cigarettes_per_day)] <- 3
  smoke_adjustments[!is.na(is_smoker) & is_smoker & !is.na(smokes_cigarettes_per_day) & smokes_cigarettes_per_day > 19] <- 6
  smoke_adjustments <- smoke_adjustments * -1

  value + altitude_adjustments + smoke_adjustments
}

is_implausible_anaemia <- function(value) {
  value < 40 | value > 200
}

anaemia_implausible_values <- function(value, age = NULL, sex = NULL) {
  value <- vec_cast(value, double())
  value[is_implausible_anaemia(value)] <- NA_real_
  value
}

#' @include indicators.R
#' @noRd
anaemia_indicator <- indicator(
  name = "Anaemia - Haemoglobin (g/L)",
  abbreviated_name = "anaemia",
  value_concept = "haemoglobin",
  export_value_name = "hgb",
  required_concepts = c(
    "sex", "age", "pregnancy_status", "altitude",
    "is_smoker", "smokes_cigarettes_per_day", "pregnancyweeks", "pregnancymonths"
  ),
  global_condition = age_in_years(age) >= 0, # no restrictions
  categories = list(
    # category(
    #     name = "Total anaemia",
    #     #value > 50
    #     (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
    #       age_in_months(age) < 60 &
    #       value < 110,
    #
    #     (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
    #       age_in_years(age) >= 5 & age_in_years(age) <= 11 &
    #       value < 115,
    #
    #     (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
    #       age_in_years(age) >= 12 & age_in_years(age) <= 14 &
    #       value < 120,
    #
    #     is_male(sex) &
    #       age_in_years(age) >= 15 &
    #       value < 130,
    #
    #     is_female(sex) &
    #       (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
    #       age_in_years(age) >= 15 &
    #       value < 120,
    #
    #     # female
    #     is_pregnant(pregnancy_status) &
    #       value < 110
    # ),
    category(
      name = "No anaemia",

      # f_dev(pregnancyweeks, pregnancymonths),

      age_in_months(age) < 24 &
        value >= 105,
      age_in_months(age) >= 24 & age_in_months(age) < 60 &
        value >= 110,
      (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
        age_in_years(age) >= 5 & age_in_years(age) <= 11 &
        value >= 115,
      (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
        age_in_years(age) >= 12 & age_in_years(age) <= 14 &
        value >= 120,
      is_female(sex) &
        (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
        age_in_years(age) >= 15 &
        value >= 120,
      is_male(sex) &
        age_in_years(age) >= 15 &
        value >= 130,
      is_female(sex) &
        is_pregnant(pregnancy_status) &
        what_trimester(pregnancyweeks, pregnancymonths) %in% c("First", "Third", "Unknown") &
        value >= 110,
      is_female(sex) &
        is_pregnant(pregnancy_status) &
        what_trimester(pregnancyweeks, pregnancymonths) == "Second" &
        value >= 105
    ),
    category(
      name = "Mild anaemia",
      age_in_months(age) < 24 &
        value >= 95 & value < 105,
      age_in_months(age) >= 24 & age_in_months(age) < 60 &
        value >= 100 & value < 110,
      (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
        age_in_years(age) >= 5 & age_in_years(age) <= 11 &
        value >= 110 & value < 115,
      (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
        age_in_years(age) >= 12 & age_in_years(age) <= 14 &
        value >= 110 & value < 120,
      is_female(sex) &
        (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
        age_in_years(age) >= 15 &
        value >= 110 & value < 120,
      is_male(sex) &
        age_in_years(age) >= 15 &
        value >= 110 & value < 130,
      is_female(sex) &
        is_pregnant(pregnancy_status) &
        what_trimester(pregnancyweeks, pregnancymonths) %in% c("First", "Third", "Unknown") &
        value >= 100 & value < 110,
      is_female(sex) &
        is_pregnant(pregnancy_status) &
        what_trimester(pregnancyweeks, pregnancymonths) == "Second" &
        value >= 95 & value < 105
    ),
    category(
      name = "Moderate anaemia",
      # value > 50
      age_in_months(age) < 24 &
        value >= 70 & value < 95,
      age_in_months(age) >= 24 & age_in_months(age) < 60 &
        value >= 70 & value < 100,
      (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
        age_in_years(age) >= 5 & age_in_years(age) <= 11 &
        value >= 80 & value < 110,
      (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
        age_in_years(age) >= 12 & age_in_years(age) <= 14 &
        value >= 80 & value < 110,
      (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
        age_in_years(age) >= 15 &
        value >= 80 & value < 110,
      is_female(sex) &
        is_pregnant(pregnancy_status) &
        what_trimester(pregnancyweeks, pregnancymonths) %in% c("First", "Third", "Unknown") &
        value >= 70 & value < 100,
      is_female(sex) &
        is_pregnant(pregnancy_status) &
        what_trimester(pregnancyweeks, pregnancymonths) == "Second" &
        value >= 70 & value < 95
    ),
    category(
      name = "Severe anaemia",
      age_in_months(age) < 60 &
        value < 70,
      (!is_pregnant(pregnancy_status) | is.na(is_pregnant(pregnancy_status))) &
        age_in_years(age) >= 5 &
        value < 80,
      is_female(sex) &
        is_pregnant(pregnancy_status) &
        value < 70
    )
  ),
  adjustment = adjustment(
    required_concepts = c(
      "altitude",
      "is_smoker", "smokes_cigarettes_per_day"
    ),
    fun = anaemia_adjustment
  ),
  implausible_values = adjustment(
    required_concepts = c(
      "age", "sex"
    ),
    fun = anaemia_implausible_values
  ),
  prev_value_cutoffs = lapply(seq(180, 40, by = -5), function(x) {
    new_prev_cutoff(
      new_function(pairlist2(value = ), bquote(value < .(x))),
      paste0("hgb_p", x)
    )
  }),
  prevalence_categories = list(
    mildanaemia = \(x) ifelse(is.na(x), NA, x %in% "Mild anaemia"),
    moderateanaemia = \(x) ifelse(is.na(x), NA, x %in% "Moderate anaemia"),
    severeanaemia = \(x) ifelse(is.na(x), NA, x %in% "Severe anaemia")
  ),
  aggregate_prevalence_categories = list(
    totalanaemia = \(x) ifelse(is.na(x), NA, x %in% c("Mild anaemia", "Moderate anaemia", "Severe anaemia"))
  ),
  prevalence_category_names = c(
    mildanaemia = "Mild anaemia",
    moderateanaemia = "Moderate anaemia",
    severeanaemia = "Severe anaemia",
    totalanaemia = "Total anaemia"
  ),
  drop_columns = list(
    short = NULL,
    long = NULL
  ),
  rename_columns = list(
    short = NULL,
    long = NULL
  ),
  reorder_columns = list(
    short = NULL,
    long = NULL
  ),
  plot_settings = list(
    dot_plot = list(
      show_ci = TRUE
    )
  )
)
