# ida_classification_tables maps ferritin classification to the respective IDA classification.
# note that some relationships are implicit here.
# 1: No anaemia => 'No iron deficiency'
# 2: Anything not in this table with anaemia is classified as 'No iron deficiency'
ida_classification_tables <- list(
  cutoff = c(
    "Iron deficiency in apparently healthy individuals" = "Iron deficiency anaemia in apparently healthy individuals",
    "Iron deficiency in individuals with infection or inflammation" = "Iron deficiency anaemia in individuals with infection or inflammation"
  ),
  rm_agp_crp = c(
    "Iron deficiency in apparently healthy individuals" = "Iron deficiency anaemia in apparently healthy individuals"
  ),
  default = c(
    "Iron deficiency in apparently healthy individuals" = "Iron deficiency anaemia in apparently healthy individuals"
  )
)

ida_levels <- c(
  "No iron deficiency anaemia",
  "Iron deficiency anaemia in apparently healthy individuals",
  "Iron deficiency anaemia in individuals with infection or inflammation"
)


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
    ferritin <- ferritin[[1]]

    # compute the composite results
    # wee first get the individual results
    anaemia_results <- indicator_results[["anaemia"]][["anaemia_result"]]
    ferritin_categories <- "Iron deficiency"
    ferritin_results <- indicator_results[[ferritin_name]][[paste0(
      ferritin_name,
      "_result"
    )]]
    stopifnot(
      length(ferritin_results) == length(anaemia_results)
    )

    # by default, all anaemia results are 'No iron deficiency anaemia'
    ida_results <- rep.int(
      "No iron deficiency anaemia",
      length(ferritin_results)
    )

    # Let's proceed with the classification
    # If any result is NA, the ida classification is NA
    is_na <- is.na(anaemia_results) | is.na(ferritin_results)
    ida_results[is_na] <- NA_character_

    # We have to have anaemia to classify anything
    anaemia_categories <- indicator_prevalence_categories(anaemia)
    is_anaemia <- !is_no_anaemia(anaemia_results)

    # now we need the right classification mapping based on the adjustment
    # of ferritin
    adj_name <- adjustment_name(ferritin$adjustment)
    classifier <- if (
      is.null(adj_name) || !adj_name %in% names(ida_classification_tables)
    ) {
      ida_classification_tables$default
    } else {
      ida_classification_tables[[adj_name]]
    }
    classification <- classifier[as.character(ferritin_results)]
    has_class <- !is.na(classification)
    ida_results[is_anaemia & !is_na & has_class] <- classification[
      is_anaemia & !is_na & has_class
    ]
    ida_results <- factor(ida_results, levels = ida_levels)

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
  name = "Iron deficiency anaemia",
  abbreviated_name = "ida_unadj",
  compute_function = ida_compute("unadj"),
  prevalence_categories = ida_prev_categories("unadj"),
  requires_indicators = c("ferritin_unadj", "anaemia")
)

ida_indicator_adjusted <- composite_indicator(
  name = "Iron deficiency anaemia",
  abbreviated_name = "ida_adj",
  compute_function = ida_compute("adj"),
  prevalence_categories = ida_prev_categories("adj"),
  requires_indicators = c("ferritin_adj", "anaemia")
)
