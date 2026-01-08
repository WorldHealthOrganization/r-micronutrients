ida_compute <- function(type) {
  function(indicator, indicators, indicator_results) {
    anaemia <- Filter(
      \(x) indicator_abbreviated_name(x) == "anaemia",
      indicators
    )
    stopifnot(length(anaemia) == 1)
    anaemia <- anaemia[[1]]
    ferritin_name <- paste0("ferritin_", type)
    ferritin <- Filter(
      \(x) indicator_abbreviated_name(x) == ferritin_name,
      indicators
    )
    stopifnot(length(ferritin) == 1)
    ferrtin <- ferritin[[1]]

    # compute the composite results
    anaemia_results <- indicator_results[["anaemia"]][["anaemia_result"]]
    anaemia_categories <- indicator_prevalence_categories(anaemia)
    is_anaemia <- anaemia_results %in% anaemia_categories
    ferritin_categories <- "Iron deficiency"
    ferritin_results <- indicator_results[[ferritin_name]][[paste0(
      ferritin_name,
      "_result"
    )]]
    is_iron_deficiency <- grepl("Iron deficiency", ferritin_results)
    stopifnot(
      length(ferritin_results) == length(anaemia_results)
    )
    is_na <- is.na(anaemia_results) | is.na(ferritin_results)
    ida_results <- rep.int(
      "No iron deficiency anaemia",
      length(ferritin_results)
    )
    ida_results[is_na] <- NA_character_
    is_valid <- !is_na & is_iron_deficiency & is_anaemia
    ida_results[is_valid] <- gsub(
      "Iron deficiency",
      "Iron deficiency anaemia",
      ferritin[is_valid]
    )

    # modify the results
    ida_short_name <- indicator_abbreviated_name(indicator)
    indicator_results[[ida_short_name]][[paste0(
      ida_short_name,
      "_result"
    )]] <- ida_results
    indicator_results
  }
}

ida_prev_categories <- function(type) {
  categories <- list()
  categories[[paste0("ida_", type)]] <- \(x) {
    ifelse(
      is.na(x),
      NA,
      x %in%
        c(
          "Iron deficiency anaemia in apparently healthy individuals",
          "Iron deficiency anaemia in individuals with infection or inflammation"
        )
    )
  }
  categories
}

ida_indicator_unadjusted <- composite_indicator(
  name = "Iron deficiency anaemia (unadjusted)",
  abbreviated_name = "ida_unadj",
  compute_function = ida_compute("unadj"),
  prevalence_categories = ida_prev_categories("unadj"),
  requires_indicators = c("ferritin_unadj", "anaemia")
)

ida_indicator_adjusted <- composite_indicator(
  name = "Iron deficiency anaemia (adjusted)",
  abbreviated_name = "ida_adj",
  compute_function = ida_compute("adj"),
  prevalence_categories = ida_prev_categories("adj"),
  requires_indicators = c("ferritin_adj", "anaemia")
)
