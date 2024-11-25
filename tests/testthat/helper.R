set.seed(1)

#' Generates a random dataset of testing. Only useful for testing the mechanics.
random_datset <- function(n) {
  data.frame(
    age_years = pmax(0, rnorm(n, mean = 20, sd = 10)),
    sex = sample(c(1, 2), size = n, replace = TRUE, prob = c(0.1, 0.9)),
    pregnancy_status = sample(c(1L, 2L, NA_integer_), size = n, replace = TRUE, prob = c(0.1, 0.89, 0.01)),
    pregnancyweeks = sample(1:(9 * 4), size = n, replace = TRUE),
    pregnancymonths = sample(1:9, size = n, replace = TRUE),
    lactating_status = sample(c(1L, 2L, NA_integer_), size = n, replace = TRUE, prob = c(0.4, 0.1, 0.5)),
    ferritin_measurement = pmax(0.1, rnorm(n, mean = 30, sd = 25)),
    crp_measurement = pmax(0.1, rnorm(n, mean = 2, sd = 10)),
    agp_measurement = pmax(0.1, rnorm(n, mean = 0.5, sd = 0.5)),
    haemoglobin_measurement = pmax(0.1, rnorm(n, mean = 100, sd = 50)),
    is_smoker = sample(c(1L, 2L, NA_integer_), size = n, replace = TRUE, prob = c(0.1, 0.89, 0.01)),
    smokes_cigarettes_per_day = rpois(n, 3),
    altitude = pmax(0, rnorm(n, mean = 500, sd = 1000)),
    malaria = sample(c(1L, 2L, NA_integer_), size = n, replace = TRUE, prob = c(0.1, 0.89, 0.01)),
    iodine = pmax(0, rnorm(n, mean = 50, sd = 100)),
    wealth_quintile = sample(paste0("Q", 1:5), size = n, replace = TRUE),
    mothers_education = sample(0:3, size = n, replace = TRUE),
    area = sample(c("rural", "urban"), size = n, replace = TRUE),
    region = sample(c("1", "2"), size = n, replace = TRUE),
    other_region = sample(c("A", "B"), size = n, replace = TRUE),
    other_grouping_variable = sample(c("C", "D"), size = n, replace = TRUE),
    team = sample(0:5, size = n, replace = TRUE)
  )
}
