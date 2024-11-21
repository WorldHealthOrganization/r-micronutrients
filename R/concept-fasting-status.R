fasting_status_levels <- c(
  "Morning, fasting",
  "Morning, non fasting",
  "Afternoon, non fasting"
)
fasting_status_morning_fasting <- factor(
  fasting_status_levels[1],
  levels = fasting_status_levels
)
fasting_status_morning_non_fasting <- factor(
  fasting_status_levels[2],
  levels = fasting_status_levels
)
fasting_status_afternoon_non_fasting <- factor(
  fasting_status_levels[3],
  levels = fasting_status_levels
)

is_fasting_status_afternoon_non_fasting <- function(x) {
  !is.na(x) & x == fasting_status_afternoon_non_fasting
}

is_fasting_status_morning_non_fasting <- function(x) {
  !is.na(x) & x == fasting_status_morning_non_fasting
}

is_fasting_status_morning_fasting <- function(x) {
  !is.na(x) & x == fasting_status_morning_fasting
}
