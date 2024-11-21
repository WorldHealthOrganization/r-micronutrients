mothers_education_levels <- c(
  "No Education",
  "Primary Education",
  "Secondary Education",
  "Higher education"
)

as_mothers_education <- function(x) {
  x <- vec_cast(x, integer())
  stopifnot(all(x %in% c(0:3, NA_integer_)))
  res <- rep.int(NA_character_, length(x))
  for (i in 0:3) {
    res[!is.na(x) & x == i] <- mothers_education_levels[i + 1]
  }
  factor(res, levels = mothers_education_levels, ordered = TRUE)
}
