pregnancy_status_levels <- c("Pregnant", "Not Pregnant")

pregnancy_status_pregnant <- factor(
  pregnancy_status_levels[1],
  levels = pregnancy_status_levels
)

pregnancy_status_not_pregnant <- factor(
  pregnancy_status_levels[2],
  levels = pregnancy_status_levels
)

is_not_pregnant <- function(pregnancy_status) {
  # !is.na(pregnancy_status) &
  pregnancy_status == pregnancy_status_not_pregnant
}
is_pregnant <- function(pregnancy_status) {
  # !is.na(pregnancy_status) &
  pregnancy_status == pregnancy_status_pregnant
}

# has_pregnancy_duration <- function(){
#   n <- length(pregnancy_weeks)
#   week_na <- sum(is.na(pregnancy_weeks))
#   month_na <- sum(is.na(pregnancy_months))
# }

what_trimester <- function(pregnancy_weeks, pregnancy_months) {
  n <- length(pregnancy_weeks)
  week_na <- sum(is.na(pregnancy_weeks))
  month_na <- sum(is.na(pregnancy_months))

  if (week_na == n && month_na == n) {
    return(rep("Unknown", n))
  }

  preg_duration_weeks <- pregnancy_weeks

  if (week_na > month_na) {
    preg_duration_weeks <- pregnancy_months * 4.34821
  }

  dplyr::case_when(
    preg_duration_weeks <= 12 ~ "First",
    preg_duration_weeks >= 13 & preg_duration_weeks <= 28 ~ "Second",
    preg_duration_weeks >= 29 ~ "Third",
    TRUE ~ "Unknown"
  )
}
