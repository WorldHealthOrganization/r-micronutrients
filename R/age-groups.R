# the below functions are mainly used for age filtering/stratification
# in data quality components

total_age_group_names <- list(
  "age_group_months_0_60" = "Age group total (months): [0-60)",
  "age_group_years_5_20" = "Age group total (years): [5-20)",
  "age_group_years_5_12" = "Age group total (years): [5-12)",
  "age_group_years_6_13" = "Age group total (years): [6-13)",
  "age_group_years_10_20" = "Age group total (years): [10-20)",
  "age_group_years_10_15" = "Age group total (years): [10-15)",
  "age_group_years_15_20" = "Age group total (years): [15-20)",
  "age_group_years_15_50" = "Age group total (years): [15-50)",
  "age_group_years_15_90" = "Age group total (years): [15-90)",
  "age_group_years_60_plus" = "Age group total (years): 60+",
  "age_group_years_90_plus" = "Age group total (years): 90+"
)

age_between <- function(unit, lower, upper) {
  function(x) {
    x <- as.numeric(x, unit)
    !is.na(x) & x >= lower & x < upper
  }
}

total_age_group_functions <- list(
  "age_group_months_0_60" = age_between("months", 0, 60),
  "age_group_years_5_20" = age_between("years", 5, 20),
  "age_group_years_5_12" = age_between("years", 5, 12),
  "age_group_years_6_13" = age_between("years", 6, 13),
  "age_group_years_10_20" = age_between("years", 10, 20),
  "age_group_years_10_15" = age_between("years", 10, 15),
  "age_group_years_15_20" = age_between("years", 15, 20),
  "age_group_years_15_50" = age_between("years", 15, 50),
  "age_group_years_15_90" = age_between("years", 15, 90),
  "age_group_years_60_plus" = age_between("years", 60, Inf),
  "age_group_years_90_plus" = age_between("years", 90, Inf)
)

apply_total_age_group_stratifications <- function(data) {
  stopifnot(is.data.frame(data), "age" %in% colnames(data))
  for (s in names(total_age_group_functions)) {
    data[[s]] <- total_age_group_functions[[s]](data[["age"]])
  }
  data
}

# the below functions are mainly used in the prevalence calculations

age_group_years_15_90 <- function(x) {
  x <- as.numeric(x, "years")
  cut(x, c(15, 20, 30, 40, 50, 60, 70, 80, 90), right = FALSE)
}

age_group_years_plus <- function(limit) {
  function(x) {
    x <- as.numeric(x, "years")
    res <- rep.int(NA_character_, length(x))
    res[x >= limit] <- paste0(limit, "+ y")
    res
  }
}

age_group_years_interval <- function(...) {
  interval <- c(...)
  stopifnot(length(interval) == 2)
  function(x) {
    x <- as.numeric(x, "years")
    cut(x, interval, right = FALSE) |>
      pad_cutpoints()
  }
}

age_group_months_0_60_g1 <- function(x) {
  x <- as.numeric(x, "months")
  cut(x, c(0, 1, 5, 60), right = FALSE) |>
    pad_cutpoints()
}

age_group_months_0_60_g2 <- function(x) {
  x <- as.numeric(x, "months")
  cut(x, c(6, 24, 60), right = FALSE) |>
    pad_cutpoints()
}

age_group_months_0_60_g3 <- function(x) {
  x <- as.numeric(x, "months")
  cut(x, c(6, 12, 24, 36, 48, 60), right = FALSE) |>
    pad_cutpoints()
}

apply_age_group_stratifications <- function(data) {
  stopifnot(is.data.frame(data), "age" %in% colnames(data))
  # age months
  data[["strat_age_group_months_0_60_g1"]] <- age_group_months_0_60_g1(data[["age"]])
  data[["strat_age_group_months_0_60_g2"]] <- age_group_months_0_60_g2(data[["age"]])
  data[["strat_age_group_months_0_60_g3"]] <- age_group_months_0_60_g3(data[["age"]])
  # age total
  data[["strat_age_group_years_5_20"]] <- age_group_years_interval(5, 20)(data[["age"]])
  data[["strat_age_group_years_5_12"]] <- age_group_years_interval(5, 12)(data[["age"]])
  data[["strat_age_group_years_6_13"]] <- age_group_years_interval(6, 13)(data[["age"]])
  data[["strat_age_group_years_10_20"]] <- age_group_years_interval(10, 20)(data[["age"]])
  data[["strat_age_group_years_10_15"]] <- age_group_years_interval(10, 15)(data[["age"]])
  data[["strat_age_group_years_15_20"]] <- age_group_years_interval(15, 20)(data[["age"]])
  data[["strat_age_group_years_15_50"]] <- age_group_years_interval(15, 50)(data[["age"]])
  # age 15 to 90 in year increments
  data[["strat_age_group_years_15_90"]] <- age_group_years_15_90(data[["age"]])
  # age group X+
  data[["strat_age_group_years_60_plus"]] <- age_group_years_plus(60)(data[["age"]])
  data[["strat_age_group_years_90_plus"]] <- age_group_years_plus(90)(data[["age"]])
  data
}

age_year_label <- \(x) paste0("Age group (years): ", x)

age_month_label <- \(x) paste0("Age group (months): ", x)
# age_year_total_label <- \(x) paste0("Age group total (years): ", x)
# age_month_total_label <- \(x) paste0("Age group total (months): ", x)
