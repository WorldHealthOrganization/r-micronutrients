# utils::globalVariables(c(
#   "CRP", "AGP", "age", "value",
#   "%between%", "pregnancy_status", "sex"
# ))

iron_deficiency_adjustment <- function(value, ...) {
  value
}

iron_deficiency_implausible <- function(value, ...) {
  value
}

#' @include indicators.R
#' @noRd
ida_indicator <- function(ferritin_adjustment = no_adjustment) {
  short_name <- "ida"
  prev_categories <- "ida"

  if (is_no_adjustment(ferritin_adjustment)) {
    short_name <- paste0(short_name, "_unadj")
    prev_categories <- paste0(prev_categories, "_unadj")
  } else {
    short_name <- paste0(short_name, "_adj")
    prev_categories <- paste0(prev_categories, "_adj")
  }

  inflammation <- function(CRP, AGP) {
    is_ferritin_cutoff_adjustment(ferritin_adjustment) &
      ((!is.na(CRP) & CRP >= 5) | (!is.na(AGP) & AGP >= 1))
  }

  ida_indicator <- indicator(
    name = "Iron deficiency anaemia",
    abbreviated_name = short_name,
    value_concept = "ida",
    export_value_name = short_name,
    required_concepts = c(
      "haemoglobin",
      "ferritin",
      "CRP",
      "AGP",
      "age",
      "pregnancy_status"
    ),
    global_condition = age_in_years(age) >= 0,
    categories = list(
      category(
        name = "Placeholder",
        # "value" in this case is actually ferritin, since all of ida's adjustments
        # are actually applied to ferritin
        #is_no_adjustment(ferritin_adjustment) &

        TRUE
        # All kids under 5 years (60 months) without inflammation
        # !inflammation(CRP, AGP) &
        #   age_in_months(age) < 60 &
        #   value >= 110 &
        #   ferritin < 12,
        #
        # # All kids 5-11
        # !inflammation(CRP, AGP) &
        #   age_in_years(age) >= 5 & age_in_years(age) <= 11 &
        #   value >= 115 &
        #   ferritin < 15,
        #
        # # Males 12-14
        # !inflammation(CRP, AGP) &
        #   is_male(sex) &
        #   age_in_years(age) >= 12 & age_in_years(age) <= 14 &
        #   value >= 120 &
        #   ferritin < 15,
        #
        # # Females 12-14, non-pregnant
        # !inflammation(CRP, AGP) &
        #   is_female(sex) &
        #   (is.na(is_pregnant(pregnancy_status)) | !is_pregnant(pregnancy_status)) &
        #   age_in_years(age) >= 12 & age_in_years(age) <= 14  &
        #   value >= 115 &
        #   ferritin < 15,
        #
        # # Males 15 and older
        # !inflammation(CRP, AGP) &
        #   is_male(sex) &
        #   age_in_years(age) >= 15  &
        #   value >= 130 &
        #   ferritin < 15,
        #
        #
        # # Females 15-49, non-pregnant
        # !inflammation(CRP, AGP) &
        #   is_female(sex) &
        #   (is.na(is_pregnant(pregnancy_status)) | !is_pregnant(pregnancy_status)) &
        #   age_in_years(age) >= 15 &
        #   value >= 120 & # this might be 120 the Word doc has two values
        #   ferritin < 15,
        #
        # # Females 15-49, pregnant
        # !inflammation(CRP, AGP) &
        #   is_female(sex) &
        #   (!is.na(is_pregnant(pregnancy_status)) & is_pregnant(pregnancy_status)) &
        #   # age_in_years(age) >= 15 & age_in_years(age) <= 49  &
        #   value >= 110 & # this might be 120 the Word doc has two values
        #   ferritin < 15
      ) #,
      # category(
      #   name = "Iron deficiency anaemia in individuals with infection or inflammation",
      #   # "value" in this case is actually ferritin, since all of ida's adjustments
      #   # are actually applied to ferritin
      #
      #   # All kids under 5 years (60 months)
      #   inflammation(CRP, AGP) &
      #     age_in_months(age) < 60 &
      #     value >= 110 &
      #     ferritin < 30,
      #
      #   # All kids 5-11
      #   inflammation(CRP, AGP) &
      #     age_in_years(age) >= 5 & age_in_years(age) <= 11 &
      #     value >= 115 &
      #     ferritin < 70,
      #
      #   # Males 12-14
      #   inflammation(CRP, AGP) &
      #     is_male(sex) &
      #     age_in_years(age) >= 12 & age_in_years(age) <= 14 &
      #     value >= 120 &
      #     ferritin < 70,
      #
      #   # Females 12-14, non-pregnant
      #   inflammation(CRP, AGP) &
      #     is_female(sex) &
      #     (is.na(is_pregnant(pregnancy_status)) | !is_pregnant(pregnancy_status)) &
      #     age_in_years(age) >= 12 & age_in_years(age) <= 14  &
      #     value >= 115 &
      #     ferritin < 70,
      #
      #   # Males 15 and older
      #   inflammation(CRP, AGP) &
      #     is_male(sex) &
      #     age_in_years(age) >= 15  &
      #     value >= 130 &
      #     ferritin < 70,
      #
      #   # Females 15-49, non-pregnant
      #   inflammation(CRP, AGP) &
      #     is_female(sex) &
      #     #(is.na(is_pregnant(pregnancy_status)) | !is_pregnant(pregnancy_status)) &
      #     age_in_years(age) >= 15 &
      #     #age_in_years(age) >= 15 & age_in_years(age) <= 49  &
      #     value >= 120 & # this might be 120 the Word doc has two values
      #     ferritin < 70
      #
      #   # Females 15-49, pregnant. This value is n/a
      #   # inflammation(CRP, AGP) &
      #   #   is_female(sex) &
      #   #   (!is.na(is_pregnant(pregnancy_status)) & is_pregnant(pregnancy_status)) &
      #   #   age_in_years(age) >= 15 & age_in_years(age) <= 49  &
      #   #   haemoglobin >= 110 & # this might be 120 the Word doc has two values
      #   #   value < 15
      #
      #   # Females 50 and older, non-pregnant. This is not defined in doc
      #   # inflammation(CRP, AGP) &
      #   #   is_female(sex) &
      #   #   (is.na(is_pregnant(pregnancy_status)) | !is_pregnant(pregnancy_status)) &
      #   #   age_in_years(age) >= 50  &
      #   #   value >= 120 &
      #   #   ferritin < 70
      # )
    ),
    adjustment = adjustment(
      required_concepts = c(
        "altitude",
        "is_smoker",
        "smokes_cigarettes_per_day"
      ),
      fun = anaemia_adjustment
    ),
    implausible_values = adjustment(
      required_concepts = c(
        "age",
        "sex"
      ),
      fun = anaemia_implausible_values
    ),
    prev_value_cutoffs = vec_c(
      lapply(c(5, 10, 12, 15, 30, 70, 100, 150), function(x) {
        new_prev_cutoff(
          new_function(pairlist2(value = ), bquote(value < .(x))),
          paste0(short_name, "_p", x)
        )
      }),
      lapply(c(200, 500), function(x) {
        new_prev_cutoff(
          new_function(pairlist2(value = ), bquote(value > .(x))),
          paste0(short_name, "_p", x)
        )
      })
    ),
    prevalence_categories = set_names(
      list(
        \(x) {
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
      ),
      prev_categories
    ),
    drop_columns = list(
      short = c(
        "ida_unadj_mean",
        "ida_unadj_mean_sd",
        "ida_unadj_mean_ll",
        "ida_unadj_mean_ul",
        "ida_unadj_25percentile",
        "ida_unadj_50percentile",
        "ida_unadj_75percentile",
        "ida_adj_mean",
        "ida_adj_mean_sd",
        "ida_adj_mean_ll",
        "ida_adj_mean_ul",
        "ida_adj_25percentile",
        "ida_adj_50percentile",
        "ida_adj_75percentile"
      ),
      long = NULL
    ),
    rename_columns = list(
      short = NULL,
      long = NULL
    ),
    reorder_columns = list(
      short = NULL,
      long = NULL
    ),
    prevalence_reports = list(
      long = FALSE
    )
  )
}


recompute_ida <- function(iv, type, indicators) {
  i_names <- sapply(indicators, \(x) x$abbreviated_name)
  list_name <- paste0("ferritin_", type)
  var_name <- paste0("ferritin_", type, "_result")
  tmp <- data.frame(
    anaemia = iv$anaemia$anaemia_result,
    ferritin = iv[[list_name]][[var_name]]
  )
  anaemia_names <- indicators[[which(
    i_names == "anaemia"
  )]]$prevalence_category_names
  ferritin_names <- "Iron deficiency"
  tmp <- tmp |>
    dplyr::mutate(
      is_anaemia = anaemia %in% anaemia_names,
      is_iron_deficiency = grepl("Iron deficiency", ferritin),
      ida = NA,
      ida = ifelse(
        is_anaemia & is_iron_deficiency,
        gsub("Iron deficiency", "Iron deficiency anaemia", ferritin),
        "No iron deficiency anaemia"
      ),
      ida = ifelse(is.na(anaemia) | is.na(ferritin), NA, ida),
      ida_valid = ifelse(is.na(ida), NA, -99999)
    )

  list_name <- paste0("ida_", type)
  var_name <- paste0("ida_", type, "_result")
  iv[[list_name]][[var_name]] <- tmp$ida
  iv[[list_name]][[paste0(list_name, "_input_value")]] <- tmp$ida_valid
  iv
}
