# Area of residence
area_levels <- c("Urban", "Rural")

as_area <- function(x) {
  x <- tolower(as.character(x))
  stopifnot(all(x %in% c("1", "2", "urban", "rural", NA_character_)))
  x[x == "1"] <- "Urban"
  x[x == "2"] <- "Rural"
  factor(x, levels = area_levels, ordered = FALSE)
}
