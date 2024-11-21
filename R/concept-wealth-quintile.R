wealth_quintiles_levels <- c(
  "Q1 (Poorest)",
  "Q2",
  "Q3",
  "Q4",
  "Q5 (Richest)"
)

as_wealth_quintiles <- function(x) {
  if (is.character(x)) {
    x <- gsub("^Q", "", x)
    x <- as.numeric(x)
  }

  x <- vec_cast(x, integer())

  # stopifnot(all(x %in% c(1:5, NA_integer_)))

  res <- rep.int(NA_character_, length(x))

  for (i in 1:5) {
    res[!is.na(x) & x == i] <- wealth_quintiles_levels[i]
  }

  factor(res, levels = wealth_quintiles_levels, ordered = TRUE)
}

wealth_quintiles_acceptor <- function(x) {
  if (all(is.na(x))) {
    return(FALSE)
  }

  wealth_quintiles_numeric_acceptor(x) ||
    wealth_quintiles_character_acceptor(x)
}

wealth_quintiles_numeric_acceptor <- function(x) {
  is.numeric(x) &&
    all(x %in% c(1, 2, 3, 4, 5, NA_integer_), na.rm = TRUE)
}

wealth_quintiles_character_acceptor <- function(x) {
  is.character(x) &&
    all(x %in% c("Q1", "Q2", "Q3", "Q4", "Q5", NA_character_), na.rm = TRUE)
}
