lactating_status_levels <- c("Lactating", "Not Lactating")

lactating_status_lactating <- factor(
  lactating_status_levels[1],
  levels = lactating_status_levels
)

lactating_status_not_lactating <- factor(
  lactating_status_levels[2],
  levels = lactating_status_levels
)

is_not_lactating <- function(lactating_status) {
  # !is.na(lactating_status) &
  lactating_status == lactating_status_not_lactating
}
is_lactating <- function(lactating_status) {
  # !is.na(lactating_status) &
  lactating_status == lactating_status_lactating
}
