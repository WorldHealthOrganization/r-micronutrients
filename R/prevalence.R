#' Long Format Prevalence
#'
#' Long format prevalence is using a wide range of cutoffs;
#' includes prevalence estimates with corresponding standard errors and
#' confidence intervals, and summary statistics (mean and standard deviation).
#'
#' @inheritParams individual_classification
#' @param strata (Optional) A numeric vector. Each individual / household should be assigned to a strata and cluster; these design-related variables are considered by the analyses to boost the stability of estimated variance. If not provided, it will be assumed that all individuals belong to the same unique strata/cluster.
#' @param cluster (Optional) A numeric vector. Each individual / household should be assigned to a strata and cluster; these design-related variables are considered by the analyses to boost the stability of estimated variance. If not provided, it will be assumed that all individuals belong to the same unique strata/cluster. All individuals with missing cluster data will be excluded from the analysis sample.
#' Notes: The calculation of prevalence estimates requires cluster labels to be nested within each stratum; i.e. cluster labels are unique for each stratum (usually sequentially). In instances of non-nested clusters, the tool will require the user to confirm that this was done on purpose and prevalence estimates will be calculated regardless
#' @param sample_weight (Optional) A numeric vector of the sampling weight. A sampling weight must be assigned to everyone in the sample to compensate for unequal probabilities of case selection in a sample, usually owing to the design. All individuals not assigned a sampling weight should be excluded from analyses for generating micronutrient estimates but remain in the data set for reporting purposes. If sampling weights are not provided, the sample will be assumed to be self-weighted, i.e. the sampling weight equals one (unweighted analyses will be carried out).
#' @param wealth_quintile (Optional) A vector indicating the wealth quintile of individuals. Accepted values: 1, 2, 3, 4, 5; or Q1, Q2, Q3, Q4, Q5; whereby 1=poorest and 5=richest, in ascending order.
#' @param mothers_education (Optional) A vector indicating the education level of mothers, encoded numerically or categorically.
#' @param area (Optional) A vector indicating the area of residence, Accepted values: "urban" or "rural".
#' @param region (Optional) A vector specifying the region or administrative division.
#' @param other_region (Optional) A vector for alternative region groupings.
#' @param other_grouping_variable (Optional) A vector for any additional grouping variable.
#' @param team (Optional) A numeric vector specifying the team conducting the survey. Whenever provided, this variable is used for performing data quality assessment stratified to help interpretation.
#'
#' @return A long-format data frame with prevalence estimates and supporting statistics.
#'
#' @details
#' The output data frame contains rows for each grouping level (e.g., by age, sex, or other strata)
#' and a wide range of metrics, including means, standard deviations, percentiles, and confidence
#' intervals for various indicators. Percentiles and prevalence thresholds are computed for indicators
#' such as haemoglobin and ferritin, adjusted and unadjusted for inflammation where relevant.
#'
#' The function can handle additional grouping variables and stratifications, providing flexibility
#' for analyses requiring specific population segments or contexts.
#'
#' @export
compute_long_format_prevalence <- function(
    indicators,
    sex,
    age,
    pregnancy_status = NULL,
    lactating_status = NULL,
    CRP = NULL,
    AGP = NULL,
    ferritin = NULL,
    iodine = NULL,
    haemoglobin = NULL,
    altitude = NULL,
    is_smoker = NULL,
    smokes_cigarettes_per_day = NULL,
    pregnancyweeks = NULL,
    pregnancymonths = NULL,
    malaria = NULL,
    cluster = NULL,
    strata = NULL,
    sample_weight = NULL,
    wealth_quintile = NULL,
    mothers_education = NULL,
    area = NULL,
    region = NULL,
    other_region = NULL,
    other_grouping_variable = NULL,
    team = NULL) {
  compute_prevalence(
    long_format_prevalence,
    indicators,
    sex = sex,
    age = age,
    pregnancy_status = pregnancy_status,
    lactating_status = lactating_status,
    CRP = CRP,
    AGP = AGP,
    ferritin = ferritin,
    iodine = iodine,
    haemoglobin = haemoglobin,
    altitude = altitude,
    is_smoker = is_smoker,
    smokes_cigarettes_per_day = smokes_cigarettes_per_day,
    pregnancyweeks = pregnancyweeks,
    pregnancymonths = pregnancymonths,
    malaria = malaria,
    wealth_quintile = wealth_quintile,
    mothers_education = mothers_education,
    area = area,
    region = region,
    other_region = other_region,
    other_grouping_variable = other_grouping_variable,
    team = team,
    cluster = cluster,
    strata = strata,
    sample_weight = sample_weight
  )
}

#' Compute Short Format Prevalence
#'
#' Short format prevalence file according to the WHO recommended cutoffs
#' standard analysis; includes prevalence estimates with corresponding
#' standard errors and confidence intervals, and summary statistics
#' (mean and standard deviation).
#'
#' @inheritParams individual_classification
#' @inheritParams compute_long_format_prevalence
#'
#' @return A short-format data frame with summary statistics for key indicators.
#'
#' @details
#' The function provides a streamlined output compared to long-format prevalence functions, focusing on key summary metrics.
#'
#' @export
compute_short_format_prevalence <- function(
    indicators,
    sex,
    age,
    pregnancy_status = NULL,
    lactating_status = NULL,
    CRP = NULL,
    AGP = NULL,
    ferritin = NULL,
    iodine = NULL,
    haemoglobin = NULL,
    altitude = NULL,
    is_smoker = NULL,
    smokes_cigarettes_per_day = NULL,
    pregnancyweeks = NULL,
    pregnancymonths = NULL,
    malaria = NULL,
    cluster = NULL,
    strata = NULL,
    sample_weight = NULL,
    wealth_quintile = NULL,
    mothers_education = NULL,
    area = NULL,
    region = NULL,
    other_region = NULL,
    other_grouping_variable = NULL,
    team = NULL) {
  compute_prevalence(
    short_format_prevalence,
    indicators,
    sex = sex,
    age = age,
    pregnancy_status = pregnancy_status,
    lactating_status = lactating_status,
    CRP = CRP,
    AGP = AGP,
    ferritin = ferritin,
    iodine = iodine,
    haemoglobin = haemoglobin,
    altitude = altitude,
    is_smoker = is_smoker,
    smokes_cigarettes_per_day = smokes_cigarettes_per_day,
    pregnancyweeks = pregnancyweeks,
    pregnancymonths = pregnancymonths,
    malaria = malaria,
    wealth_quintile = wealth_quintile,
    mothers_education = mothers_education,
    area = area,
    region = region,
    other_region = other_region,
    other_grouping_variable = other_grouping_variable,
    team = team,
    cluster = cluster,
    strata = strata,
    sample_weight = sample_weight
  )
}

compute_prevalence <- function(
    prev_function,
    indicators,
    sex,
    age,
    pregnancy_status = NULL,
    lactating_status = NULL,
    CRP = NULL,
    AGP = NULL,
    ferritin = NULL,
    iodine = NULL,
    haemoglobin = NULL,
    altitude = NULL,
    is_smoker = NULL,
    smokes_cigarettes_per_day = NULL,
    pregnancyweeks = NULL,
    pregnancymonths = NULL,
    malaria = NULL,
    cluster = NULL,
    strata = NULL,
    sample_weight = NULL,
    wealth_quintile = NULL,
    mothers_education = NULL,
    area = NULL,
    region = NULL,
    other_region = NULL,
    other_grouping_variable = NULL,
    team = NULL) {
  validate_indicators(indicators)
  classified_data <- classify_data_internal(
    indicators = indicators,
    sex = sex, age = age, pregnancy_status = pregnancy_status,
    lactating_status = lactating_status, CRP = CRP, AGP = AGP,
    ferritin = ferritin, iodine = iodine, haemoglobin = haemoglobin,
    altitude = altitude, is_smoker = is_smoker, smokes_cigarettes_per_day = smokes_cigarettes_per_day,
    pregnancymonths = pregnancymonths, pregnancyweeks = pregnancyweeks,
    malaria = malaria,
    .format_column_names = FALSE
  )
  concept_list <- concepts_from_args(
    sex = sex,
    age = age,
    pregnancy_status = pregnancy_status,
    lactating_status = lactating_status,
    CRP = CRP,
    AGP = AGP,
    ferritin = ferritin,
    iodine = iodine,
    haemoglobin = haemoglobin,
    altitude = altitude,
    is_smoker = is_smoker,
    smokes_cigarettes_per_day = smokes_cigarettes_per_day,
    pregnancyweeks = pregnancyweeks,
    pregnancymonths = pregnancymonths,
    malaria = malaria,
    wealth_quintile = wealth_quintile,
    mothers_education = mothers_education,
    area = area,
    region = region,
    other_region = other_region,
    other_grouping_variable = other_grouping_variable,
    team = team,
    cluster = cluster,
    strata = strata,
    sample_weight = sample_weight
  )
  input_concepts <- dplyr::bind_cols(concept_list)
  stopifnot(nrow(input_concepts) == nrow(classified_data))
  survey_data <- dplyr::bind_cols(classified_data, input_concepts)
  prevalence_data <- prev_function(survey_data, indicators)
  prevalence_data
}

#' @import survey
#' @import rlang
long_format_prevalence <- function(survey_data, indicators) {
  indicators <- Filter(prevalence_report_long, indicators)

  # first we build a dataset that is used by {survey} for analysis
  survey_data <- build_prevalence_survey_data(survey_data, indicators)
  survey_df <- survey_data$data
  strat_labels <- survey_data$strat_labels
  strat_formula <- survey_data$strat_formula
  indicator_columns <- survey_data$indicator_columns
  rename_cols <- sapply(indicators, indicator_rename_columns, "long") |>
    unlist()
  reorder_cols <- sapply(indicators, indicator_reorder_columns, "long") |>
    unlist()


  design <- prevalence_design(survey_df)

  prev_age_start_end <- prevalence_age_start_end(survey_df, strat_labels)
  pop_estimates <- prevalence_pop_estimates(
    design,
    indicators,
    strat_formula
  )
  quantile_estimates <- prevalence_quantile_estimates(
    weighted(design),
    indicators,
    strat_formula
  )

  mean_estimates <- prevalence_mean_estimates(
    weighted(design),
    indicators,
    strat_formula
  )

  prev_mean_estimates <- prevalence_mean_prev_estimates(
    weighted(design),
    indicators,
    strat_formula,
    cut_off_columns(indicators)
  )

  mean_estimates_geometric <- prevalence_mean_estimates_geometric(
    weighted(design),
    indicators,
    strat_formula
  )

  combine_and_format_estimates(
    indicators,
    strat_labels,
    expected_columns = prevalence_long_format_columns(indicators),
    age_start_end = prev_age_start_end,
    rename_cols = rename_cols,
    reorder_cols = reorder_cols,
    pop_estimates,
    quantile_estimates,
    mean_estimates,
    prev_mean_estimates,
    mean_estimates_geometric
  )
}

short_format_prevalence <- function(survey_data, indicators) {
  indicators <- Filter(prevalence_report_short, indicators)

  # first we build a dataset that is used by {survey} for analysis
  survey_data <- build_prevalence_survey_data(survey_data, indicators)
  survey_df <- survey_data$data
  strat_labels <- survey_data$strat_labels
  strat_formula <- survey_data$strat_formula
  indicator_columns <- survey_data$indicator_columns

  rename_cols <- sapply(indicators, indicator_rename_columns, "short") |>
    unlist()
  reorder_cols <- sapply(indicators, indicator_reorder_columns, "short") |>
    unlist()


  design <- prevalence_design(survey_df)

  prev_age_start_end <- prevalence_age_start_end(survey_df, strat_labels)

  pop_estimates <- prevalence_pop_estimates(
    design,
    indicators,
    strat_formula
  )
  quantile_estimates <- prevalence_quantile_estimates(
    weighted(design),
    indicators,
    strat_formula
  )

  mean_estimates <- prevalence_mean_estimates(
    weighted(design),
    indicators,
    strat_formula
  )

  prev_mean_estimates <- prevalence_mean_prev_estimates(
    weighted(design),
    indicators,
    strat_formula,
    indicator_result_columns(indicators, indicator_columns)
  )

  combine_and_format_estimates(
    indicators,
    strat_labels,
    expected_columns = prevalence_short_format_columns(indicators),
    rename_cols = rename_cols,
    reorder_cols = reorder_cols,
    age_start_end = prev_age_start_end,
    pop_estimates,
    quantile_estimates,
    mean_estimates,
    prev_mean_estimates
  )
}

#' @include age-groups.R
build_prevalence_survey_data <- function(survey_df, indicators) {
  survey_df <- dplyr::mutate_if(survey_df, is_measurement, as.numeric)

  # add stratification columns
  survey_df[["strat_all"]] <- factor(ALL_VALUE)
  survey_df[["strat_sex"]] <- survey_df[["sex"]]
  survey_df <- apply_age_group_stratifications(survey_df)

  strat_labels <- list(
    "strat_all" = identity,
    "strat_sex" = \(x) paste0("Sex: ", x),
    "strat_age_group_months_0_60_g1" = age_month_label,
    "strat_age_group_months_0_60_g2" = age_month_label,
    "strat_age_group_months_0_60_g3" = age_month_label,
    "strat_age_group_years_5_20" = age_year_label,
    "strat_age_group_years_5_12" = age_year_label,
    "strat_age_group_years_6_13" = age_year_label,
    "strat_age_group_years_10_20" = age_year_label,
    "strat_age_group_years_10_15" = age_year_label,
    "strat_age_group_years_15_20" = age_year_label,
    "strat_age_group_years_15_50" = age_year_label,
    "strat_age_group_years_15_90" = age_year_label,
    "strat_age_group_years_60_plus" = age_year_label,
    "strat_age_group_years_90_plus" = age_year_label
  )

  interaction_keys <- c(
    "strat_sex",
    "strat_age_group_months_0_60_g1",
    "strat_age_group_months_0_60_g2",
    "strat_age_group_months_0_60_g3",
    "strat_age_group_years_5_20",
    "strat_age_group_years_5_12",
    "strat_age_group_years_6_13",
    "strat_age_group_years_10_20",
    "strat_age_group_years_10_15",
    "strat_age_group_years_15_20",
    "strat_age_group_years_15_50",
    "strat_age_group_years_15_90",
    "strat_age_group_years_60_plus",
    "strat_age_group_years_90_plus"
  )

  # interaction of sex and age
  for (interaction_key in setdiff(interaction_keys, "strat_sex")) {
    new_interaction_key <- paste0("strat_sex", interaction_key)
    survey_df[[new_interaction_key]] <- interaction(
      survey_df[["strat_sex"]],
      survey_df[[interaction_key]]
    )
    strat_labels[[new_interaction_key]] <- new_function(
      pairlist2(x = ),
      bquote({
        paste0("Sex + ", strat_labels[[.(interaction_key)]](x))
      })
    )
  }

  # add optional stratifications
  optional_stratification <- function(label, concept_name) {
    if (concept_name %in% colnames(survey_df)) {
      key <- paste0("strat_", concept_name)

      survey_df[[key]] <<- survey_df[[concept_name]]
      strat_labels[[key]] <<- \(x) paste0(label, ": ", x)
      for (interaction_key in interaction_keys) {
        new_interaction_key <- paste0(key, interaction_key)
        survey_df[[new_interaction_key]] <<- interaction(
          survey_df[[key]],
          survey_df[[interaction_key]]
        )
        strat_labels[[new_interaction_key]] <<- new_function(
          pairlist2(x = ),
          bquote({
            paste0(label, " + ", strat_labels[[.(interaction_key)]](x))
          })
        )
      }
    }
  }
  optional_stratification("Wealth quintile", "wealth_quintile")
  optional_stratification("Mother's education", "mothers_education")
  optional_stratification("Area", "area")
  optional_stratification("Geographical region", "region")
  optional_stratification("Other region", "other_region")
  optional_stratification("Other filter", "other_grouping_variable")
  optional_stratification("Pregnancy Status", "pregnancy_status")
  optional_stratification("Lactating Status", "lactating_status")
  optional_stratification("Team", "team")

  # only include stratifications when there is at least one observation
  strat_labels_include <- Filter(
    \(x) !all(is.na(survey_df[[x]])),
    names(strat_labels)
  )
  strat_labels <- strat_labels[strat_labels_include]

  # filter out rows with sample weight NA
  if ("sample_weight" %in% colnames(survey_df)) {
    survey_df <- survey_df[!is.na(survey_df$sample_weight), ]
  }
  # filter out all rows that have missing or negative age
  if ("age" %in% colnames(survey_df)) {
    survey_df <- survey_df[!is.na(survey_df$age) & survey_df$age >= 0, ]
  }

  # prevalence columns for cutoffs
  indicator_columns <- character()
  for (indicator in indicators) {
    indicator_name <- indicator_abbreviated_name(indicator)
    value <- survey_df[[paste0(indicator_name, "_input_value")]]
    survey_df[[paste0("prev_", indicator_name)]] <- !is.na(value)
    cutoffs <- indicator_apply_prev_cutoffs(indicator, value)
    colnames(cutoffs) <- paste0("prev_mean_", colnames(cutoffs))
    survey_df <- dplyr::bind_cols(survey_df, cutoffs)
    indicator_columns <- vec_c(indicator_columns, colnames(cutoffs))
  }

  # prevalence columns for indicator categories
  for (indicator in indicators) {
    indicator_name <- indicator_abbreviated_name(indicator)
    prev_categories <- indicator_prevalence_categories(indicator)
    agg_prev_categories <- indicator_agg_prevalence_categories(indicator)
    result <- survey_df[[paste0(indicator_name, "_result")]]
    prev_results <- as_tibble(lapply(prev_categories, \(fun) fun(result)))
    colnames(prev_results) <- paste0("prev_mean_", colnames(prev_results))
    survey_df <- dplyr::bind_cols(survey_df, prev_results)
    indicator_columns <- vec_c(indicator_columns, colnames(prev_results))

    if (!is.null(agg_prev_categories)) {
      agg_prev_results <- as_tibble(lapply(agg_prev_categories, \(fun) fun(result)))
      colnames(agg_prev_results) <- paste0("prev_mean_", colnames(agg_prev_results))
      survey_df <- dplyr::bind_cols(survey_df, agg_prev_results)
      indicator_columns <- vec_c(indicator_columns, colnames(agg_prev_results))
    }
  }

  # just some quick runtime checks for consistency
  stopifnot(
    length(indicator_columns) == length(unique(indicator_columns)),
    setequal(
      names(strat_labels),
      intersect(names(strat_labels), colnames(survey_df))
    )
  )
  list(
    data = survey_df,
    strat_labels = strat_labels,
    strat_formula = make.formula(names(strat_labels)),
    indicator_columns = indicator_columns
  )
}

combine_and_format_estimates <- function(indicators, strat_labels, expected_columns, age_start_end, rename_cols, reorder_cols, ...) {
  estimates <- list(...)
  result <- Reduce(function(acc, el) {
    dplyr::inner_join(acc, el, by = c("stratification", "stratification_type"))
  }, estimates)
  result <- dplyr::group_by(
    result,
    .data$stratification_type
  ) |>
    dplyr::mutate(
      Group = strat_labels[[.data$stratification_type[[1]]]](.data$stratification)
    )
  result <- dplyr::bind_cols(
    age_group_info_columns(
      result$stratification_type,
      result$stratification
    ),
    result
  )

  result <- dplyr::left_join(result, age_start_end,
    by = c("stratification", "stratification_type", "age_unit")
  )
  result[["stratification"]] <- NULL
  result[["stratification_type"]] <- NULL

  # depending on the data we can sometimes have some of the expected columns missing
  # this can happen if all observations are NA. In these cases we add the columns
  # with NA_real_ as values.
  common <- intersect(expected_columns, colnames(result))
  missing_cols <- setdiff(expected_columns, colnames(result))
  result <- result[, common]
  for (col in missing_cols) {
    result[[col]] <- NA_real_
  }

  if (!is.null(rename_cols)) {
    result <- result |>
      dplyr::rename(!!!rename_cols)
  }

  if (!is.null(reorder_cols)) {
    for (i in seq_along(reorder_cols)) {
      col1 <- names(reorder_cols)[i]
      bef1 <- unname(reorder_cols[i])
      result <- result |>
        dplyr::relocate(!!col1, .before = !!bef1)
    }
  }

  result
}

# this extract age group info from the stratification labels
age_group_info_columns <- function(strat_types, strat_values) {
  mapply(function(strat_type, strat_value) {
    is_age_group <- grepl("age_group", strat_type, fixed = TRUE)
    # start <- NA_integer_
    # end <- NA_integer_
    unit <- NA_character_
    if (is_age_group) {
      # is_plus <- grepl("plus", strat_type, fixed = TRUE)
      unit <- if (grepl("years", strat_type, fixed = TRUE)) {
        "years"
      } else {
        "months"
      }
      # if (is_plus) {
      #     match <- stringr::str_match(strat_value, "(\\d+)\\+\\sy")
      #     if (nrow(match) == 1 && ncol(match) == 2) {
      #         start <- as.integer(match[, 2])
      #     }
      # } else {
      #     match <- stringr::str_match(
      #         strat_value, "\\[(\\d+)\\,(\\d+)\\)"
      #     )
      #     if (nrow(match) == 1 && ncol(match) == 3) {
      #         start <- as.integer(match[, 2])
      #         end <- as.integer(match[, 3])
      #     }
      # }
    }
    tibble(
      # age_start = start,
      # age_end = end,
      age_unit = unit
    )
  }, strat_types, strat_values, SIMPLIFY = FALSE) |>
    dplyr::bind_rows()
}

prevalence_long_format_columns <- function(indicators) {
  ordered_cols <- c(
    "Group",
    "age_start",
    "age_end",
    "age_unit"
  )
  drop_cols <- NULL

  for (indicator in indicators) {
    name <- indicator_export_value_name(indicator)
    drop_cols <- c(drop_cols, indicator_drop_columns(indicator, "long"))
    cutoffs <- indicator_prev_cutoff_names(indicator)
    new_cols <- vec_c(
      paste0(name, vec_c("_pop", "_unwpop")),
      paste0(name, vec_c(
        "_mean",
        "_mean_sd",
        "_mean_ll",
        "_mean_ul",
        "_geomean",
        "_geomean_ll",
        "_geomean_ul"
      )),
      paste0(name, vec_c(
        "_10percentile",
        "_25percentile",
        "_50percentile",
        "_75percentile",
        "_90percentile"
      )),
      unlist(
        lapply(cutoffs, function(cutoff) {
          paste0(cutoff, vec_c(
            "_r", "_se", "_ll", "_ul"
          ))
        })
      )
    )

    ordered_cols <- vec_c(
      ordered_cols,
      new_cols
    )
  }

  ordered_cols[!(ordered_cols %in% drop_cols)]
}

prevalence_short_format_columns <- function(indicators) {
  ordered_cols <- c(
    "Group",
    "age_start",
    "age_end",
    "age_unit"
  )
  drop_cols <- NULL
  # rename_cols <- NULL

  for (indicator in indicators) {
    name <- indicator_export_value_name(indicator)
    drop_cols <- c(drop_cols, indicator_drop_columns(indicator, "short"))
    # rename_cols <- c(rename_cols, indicator_rename_columns(indicator, "short"))
    prev_cats <- names(indicator_prevalence_categories(indicator))
    agg_prev_cats <- names(indicator_agg_prevalence_categories(indicator))
    prev_cats <- c(prev_cats, agg_prev_cats)
    new_cols <- vec_c(
      paste0(name, vec_c("_pop", "_unwpop")),
      paste0(name, vec_c(
        "_mean",
        "_mean_sd",
        "_mean_ll",
        "_mean_ul"
      )),
      paste0(name, vec_c(
        "_25percentile",
        "_50percentile",
        "_75percentile",
      )),
      unlist(
        lapply(prev_cats, function(prev_cat) {
          paste0(prev_cat, vec_c(
            "_r", "_se", "_ll", "_ul"
          ))
        })
      )
    )
    ordered_cols <- vec_c(
      ordered_cols,
      new_cols
    )
  }

  # if(!is.null(rename_cols)){
  #   ordered_cols[match(rename_cols, ordered_cols, nomatch = 0)] <- names(rename_cols)
  # }

  ordered_cols[!ordered_cols %in% drop_cols]
}

prevalence_design <- function(analysis_df) {
  # survey configuration
  cluster_formula <- if ("cluster" %in% colnames(analysis_df)) {
    ~cluster
  } else {
    ~1
  }
  strata_formula <- if ("strata" %in% colnames(analysis_df)) {
    ~strata
  }
  weights_formula <- if ("sample_weight" %in% colnames(analysis_df)) {
    ~sample_weight
  }
  unweighted_design <- svydesign(
    ids = cluster_formula,
    strata = strata_formula,
    weights = NULL,
    data = analysis_df,
    nest = TRUE
  )
  weighted_design <- svydesign(
    ids = cluster_formula,
    strata = strata_formula,
    weights = weights_formula,
    data = analysis_df,
    nest = TRUE
  )
  list(
    weighted = weighted_design,
    unweighted = unweighted_design
  )
}

prevalence_quantile_estimates <- function(weighted_design, indicators, stratification_formula) {
  format_strat <- function(indicator, strat_df) {
    value_name <- indicator_export_value_name(indicator)
    indicator_name <- paste0(indicator_abbreviated_name(indicator), "_input_value")
    res <- init_stratified_result(strat_df)
    res[[paste0(value_name, "_10percentile")]] <- strat_df[[paste0(indicator_name, ".0.1")]]
    res[[paste0(value_name, "_25percentile")]] <- strat_df[[paste0(indicator_name, ".0.25")]]
    res[[paste0(value_name, "_50percentile")]] <- strat_df[[paste0(indicator_name, ".0.5")]]
    res[[paste0(value_name, "_75percentile")]] <- strat_df[[paste0(indicator_name, ".0.75")]]
    res[[paste0(value_name, "_90percentile")]] <- strat_df[[paste0(indicator_name, ".0.9")]]
    res
  }
  results <- lapply(seq_along(indicators), function(i) {
    indicator_name <- paste0(indicator_abbreviated_name(indicators[[i]]), "_input_value")
    value_formula <- make.formula(indicator_name)
    weighted_est <- robust_svybys(
      formula = value_formula,
      bys = stratification_formula,
      design = weighted_design,
      FUN = svyquantile,
      df = degf(weighted_design),
      quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90),
      drop.empty.groups = FALSE,
      na.rm = TRUE,
      na.rm.all = TRUE
    )
    dplyr::bind_rows(lapply(weighted_est, \(x) format_strat(indicators[[i]], x)))
  })
  combine_stratified_results(results)
}

#' @importFrom tibble tibble
#' @importFrom stats confint
prevalence_mean_estimates <- function(weighted_design, indicators, stratification_formula) {
  results <- lapply(seq_along(indicators), function(i) {
    indicator_name <- paste0(indicator_abbreviated_name(indicators[[i]]), "_input_value")
    value_formula <- make.formula(indicator_name)
    mean_est <- robust_svybys(
      formula = value_formula,
      bys = stratification_formula,
      design = weighted_design,
      FUN = svymean,
      drop.empty.groups = FALSE,
      na.rm = TRUE,
      na.rm.all = TRUE
    )
    sd_est <- robust_svybys(
      formula = value_formula,
      bys = stratification_formula,
      design = weighted_design,
      FUN = robust_svyvar(indicator_name),
      drop.empty.groups = FALSE,
      na.rm = TRUE,
      na.rm.all = TRUE
    )
    mapply(function(mean_est, sd_est) {
      if (ncol(mean_est) == 1 || ncol(sd_est) == 1) {
        # an error happened during the computation
        mean <- mean_sd <- mean_ll <- mean_ul <- NA_real_
      } else {
        ci <- confint(mean_est, level = 0.95, df = degf(weighted_design))
        mean <- mean_est[[indicator_name]]
        # sometimes the column can be named "V1" instead of <indicator_name>
        # so we select by position
        stopifnot(ncol(sd_est) == 3)
        mean_sd <- sqrt(sd_est[[2L]])
        mean_ll <- ci[, 1]
        mean_ul <- ci[, 2]
      }

      value_name <- indicator_export_value_name(indicators[[i]])
      res <- init_stratified_result(mean_est)

      res[[paste0(value_name, "_mean")]] <- mean
      res[[paste0(value_name, "_mean_sd")]] <- mean_sd
      res[[paste0(value_name, "_mean_ll")]] <- mean_ll
      res[[paste0(value_name, "_mean_ul")]] <- mean_ul
      res
    }, mean_est, sd_est, SIMPLIFY = FALSE) |>
      dplyr::bind_rows()
  })
  combine_stratified_results(results)
}


#' @importFrom tibble tibble
#' @importFrom stats confint
prevalence_mean_estimates_geometric <- function(weighted_design, indicators, stratification_formula) {
  results <- lapply(seq_along(indicators), function(i) {
    indicator_name <- paste0(indicator_abbreviated_name(indicators[[i]]), "_input_value")
    value_formula <- make.formula(paste0("log(", indicator_name, ")"))
    mean_est <- robust_svybys(
      formula = value_formula,
      bys = stratification_formula,
      design = weighted_design,
      FUN = svymean,
      drop.empty.groups = FALSE,
      na.rm = TRUE,
      na.rm.all = TRUE
    )

    sd_est <- robust_svybys(
      formula = value_formula,
      bys = stratification_formula,
      design = weighted_design,
      FUN = robust_svyvar(paste0("log(", indicator_name, ")")),
      drop.empty.groups = FALSE,
      na.rm = TRUE,
      na.rm.all = TRUE
    )

    mapply(function(mean_est, sd_est) {
      if (ncol(mean_est) == 1 || ncol(sd_est) == 1) {
        # an error happened during the computation
        mean <- mean_sd <- mean_ll <- mean_ul <- NA_real_
      } else {
        ci <- exp(confint(mean_est, level = 0.95, df = degf(weighted_design)))
        mean <- exp(mean_est[[paste0("log(", indicator_name, ")")]])
        mean_ll <- ci[, 1]
        mean_ul <- ci[, 2]
      }

      value_name <- indicator_export_value_name(indicators[[i]])
      res <- init_stratified_result(mean_est)

      res[[paste0(value_name, "_geomean")]] <- mean
      res[[paste0(value_name, "_geomean_ll")]] <- mean_ll
      res[[paste0(value_name, "_geomean_ul")]] <- mean_ul
      res
    }, mean_est, sd_est, SIMPLIFY = FALSE) |>
      dplyr::bind_rows()
  })
  combine_stratified_results(results)
}



prevalence_mean_prev_estimates <- function(weighted_design, indicators,
                                           stratification_formula, indicator_columns) {
  results <- lapply(indicator_columns, function(indicator_column) {
    value_formula <- make.formula(indicator_column)
    indicator_name <- gsub("^prev_mean_", "", indicator_column)
    mean_est <- robust_svybys(
      formula = value_formula,
      bys = stratification_formula,
      design = weighted_design,
      FUN = svymean,
      df = degf(weighted_design),
      drop.empty.groups = FALSE,
      na.rm = TRUE,
      na.rm.all = TRUE
    )
    suppressWarnings({
      # we ignore warnings from CI computation
      CI_est <- robust_svybys(
        formula = value_formula,
        bys = stratification_formula,
        design = weighted_design,
        FUN = svyciprop,
        vartype = "ci",
        df = degf(weighted_design),
        method = "mean",
        drop.empty.groups = FALSE,
        level = 0.95,
        na.rm = TRUE,
        na.rm.all = TRUE
      )
    })
    mapply(function(mean_est, ci) {
      if (ncol(mean_est) == 1 || ncol(ci) == 1) {
        # an error happened during the computation
        r <- se <- ll <- ul <- NA_real_
      } else {
        r <- mean_est[[paste0(indicator_column, "TRUE")]]
        se <- SE(mean_est)[[paste0("se.", indicator_column, "TRUE")]]
        ll <- ci[["ci_l"]]
        ul <- ci[["ci_u"]]
      }
      res <- init_stratified_result(mean_est)
      res[[paste0(indicator_name, "_r")]] <- r
      res[[paste0(indicator_name, "_se")]] <- se
      res[[paste0(indicator_name, "_ll")]] <- ll
      res[[paste0(indicator_name, "_ul")]] <- ul
      res
    }, mean_est, CI_est, SIMPLIFY = FALSE) |>
      dplyr::bind_rows()
  })
  combine_stratified_results(results)
}

# svyvar fails if there is just one observation in the group
# affects {survey} <= 4.0
robust_svyvar <- function(colname) {
  function(x, design, na.rm = FALSE, ...) {
    res <- svyvar(x, design, na.rm = TRUE, ...)
    if (length(res) == 2) {
      new_res <- NA_real_
      attr(new_res, "names") <- colname
      attr(new_res, "var") <- NA_real_
      attr(new_res, "statistic") <- "variance"
      class(new_res) <- c("svyvar", "svystat")
      return(new_res)
    }
    res
  }
}

# svyby fails it seems if all observations are NA
# therefore we create a more robust version of svybys that
# replaces failed stratifications by NA. This assumes that at least one
# stratification will not fail.
# More in Github ticket 164.
robust_svybys <- function(formula, bys, design, FUN, ...) {
  stratifications <- attr(terms(bys), "term.label")
  lapply(stratifications, function(s) {
    by <- rlang::new_formula(NULL, as.symbol(s))
    tryCatch(
      svyby(
        formula = formula,
        by = by,
        design = design,
        FUN = FUN,
        ...
      ),
      error = function(e) {
        # we compute the total here just to get the right
        # structure and ordering of the dataframe.
        svyby(
          formula = formula,
          by = by,
          design = design,
          FUN = svytotal,
          drop.empty.groups = FALSE,
          na.rm = TRUE,
          na.rm.all = FALSE
        )[, 1, drop = FALSE]
      }
    )
  })
}

combine_stratified_results <- function(list_of_dfs) {
  Reduce(function(acc, el) {
    dplyr::inner_join(acc, el, by = c("stratification", "stratification_type"))
  }, list_of_dfs)
}

weighted <- function(design) {
  design$weighted
}

unweighted <- function(design) {
  design$unweighted
}

#' @importFrom tibble as_tibble
prevalence_pop_estimates <- function(design, indicators, stratification_formula) {
  indicator_names <- lapply(indicators, indicator_abbreviated_name)
  if (length(indicator_names) == 0) {
    return(NULL)
  }
  prev_columns <- paste0("prev_", indicator_names)
  prev_formula <- make.formula(prev_columns)

  unweighted_est <- svybys(
    formula = prev_formula,
    bys = stratification_formula,
    design = unweighted(design), FUN = svytotal,
    drop.empty.groups = FALSE,
    na.rm = FALSE # there shouldn't be any NAs
  )
  weighted_est <- svybys(
    formula = prev_formula,
    bys = stratification_formula,
    design = weighted(design), FUN = svytotal,
    drop.empty.groups = FALSE,
    na.rm = FALSE # there shouldn't be any NAs
  )

  format_strat <- function(indicators, strat_df, suffix) {
    value_names <- lapply(indicators, \(x) x$export_value_name)
    res <- init_stratified_result(strat_df)
    for (i in seq_along(indicators)) {
      col <- paste0(value_names[[i]], suffix)
      res[[col]] <- strat_df[[paste0(prev_columns[[i]], "TRUE")]]
      res[[col]][is.na(res[[col]])] <- 0
    }
    res
  }
  dplyr::bind_rows(lapply(unweighted_est, \(x) format_strat(indicators, x, "_unwpop"))) |>
    dplyr::inner_join(
      dplyr::bind_rows(lapply(weighted_est, \(x) format_strat(indicators, x, "_pop"))),
      by = c("stratification", "stratification_type")
    )
}
init_stratified_result <- function(strat_df) {
  res <- as_tibble(strat_df[, 1, drop = FALSE])
  res[["stratification_type"]] <- colnames(res)
  colnames(res) <- c("stratification", "stratification_type")
  res
}

indicator_columns <- function(indicator, columns) {
  # git399
  ind_prefix <- indicator_export_value_name(indicator)
  ind_agg_prev_prefix <- names(indicator_agg_prevalence_categories(indicator))
  ind_prev_prefix <- names(indicator_prevalence_categories(indicator))

  cols_ind <- NULL
  cols_agg_prev_ind <- NULL
  cols_prev_ind <- NULL

  if (!is.null(ind_prefix)) {
    cols_ind <- columns[grepl(paste0("^", ind_prefix, collapse = "|"), columns)]
  }

  if (!is.null(ind_agg_prev_prefix)) {
    cols_agg_prev_ind <- columns[grepl(paste(ind_agg_prev_prefix, collapse = "|"), columns)]
  }

  if (!is.null(ind_prev_prefix)) {
    cols_prev_ind <- columns[grepl(paste(ind_prev_prefix, collapse = "|"), columns)]
  }

  c(cols_ind, cols_agg_prev_ind, cols_prev_ind)
}

cut_off_columns <- function(indicators, indicator_columns) {
  cols <- lapply(indicators, indicator_prev_cutoff_names) |>
    unlist()
  paste0("prev_mean_", cols)
}

indicator_result_columns <- function(indicators, indicator_columns) {
  setdiff(indicator_columns, cut_off_columns(indicators))
}

#' @importFrom rlang .data
prevalence_age_start_end <- function(analysis_df, strat_labels) {
  strat_labels_name <- names(strat_labels)
  strat_labels_name_age <- strat_labels_name[grepl("_age_", strat_labels_name)]

  res <- lapply(strat_labels_name_age, function(strat_label) {
    age_dat <- analysis_df[, c("age", strat_label)] |>
      na.exclude() |>
      dplyr::summarise(
        age_start = seconds_to_years(min(age), 1),
        age_end = seconds_to_years(max(age), 1),
        .by = dplyr::all_of(strat_label)
      ) |>
      dplyr::rename(stratification = !!strat_label) |>
      dplyr::mutate(
        stratification_type = strat_label
      )

    age_dat[, c("stratification", "stratification_type", "age_start", "age_end")]
  })

  res_year <- do.call("rbind", res)
  res_month <- res_year |>
    dplyr::mutate(
      age_start = years_to_months(.data$age_start),
      age_end = years_to_months(.data$age_end)
    )

  dplyr::bind_rows(
    res_year |> dplyr::mutate(age_unit = "years"),
    res_month |> dplyr::mutate(age_unit = "months")
  ) |>
    dplyr::mutate(
      age_start = round(.data$age_start, digits = 1),
      age_end = round(.data$age_end, digits = 1)
      # age_start = format(age_start, digits = 2, nsmall = 2),
      # age_end = format(age_end, digits = 2, nsmall = 2)
    )
}
