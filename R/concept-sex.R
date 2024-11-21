sex_levels <- c("Female", "Male")
sex_male <- factor("Male", levels = sex_levels)
sex_female <- factor("Female", levels = sex_levels)
sex_colors <- c("Female" = "#fcc66d", "Male" = "#ef8a0c")

is_male <- function(sex) {
  !is.na(sex) & sex == sex_male
}

is_female <- function(sex) {
  !is.na(sex) & sex == sex_female
}

sex_possible_male <- chr("m", "1", "male")
sex_possible_female <- chr("f", "2", "female")
sex_possible <- chr(sex_possible_male, sex_possible_female, NA_character_)

sex_acceptor <- function(x) {
  all(tolower(x) %in% sex_possible) && !all(is.na(x))
}

sex_standardizer <- function(x) {
  x <- as.character(x)
  lx <- tolower(x)

  x[lx %in% sex_possible_female] <- "Female"
  x[lx %in% sex_possible_male] <- "Male"

  stopifnot(all(x %in% c(sex_levels, NA_character_)))

  factor(x, levels = sex_levels)
}
