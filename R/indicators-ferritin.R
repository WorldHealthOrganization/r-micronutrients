ferritin_prevalence_categories <- c(
  "depletedironstores" = "Iron deficiency",
  "riskofironoverload" = "Risk of overload"
)

utils::globalVariables(c(
  "CRP",
  "AGP",
  "age",
  "value",
  "%between%",
  "pregnancy_status",
  "sex"
))

#' @include indicators.R
#' @noRd
ferritin_indicator <- function(value_adjustment = no_adjustment) {
  short_name <- "ferritin"

  prevalence_category_names <- c("depletedironstores", "riskofironoverload")

  if (is_no_adjustment(value_adjustment)) {
    short_name <- paste0(short_name, "_unadj")
    prevalence_category_names <- paste0(prevalence_category_names, "_unadj")
  } else {
    short_name <- paste0(short_name, "_adj")
    prevalence_category_names <- paste0(prevalence_category_names, "_adj")
  }

  inflammation <- function(CRP, AGP) {
    is_ferritin_cutoff_adjustment(value_adjustment) &
      ((!is.na(CRP) & CRP >= 5) | (!is.na(AGP) & AGP >= 1))
  }

  indicator(
    name = "Iron Deficiency - Ferritin (\u00B5g/L)",
    abbreviated_name = short_name,
    value_concept = "ferritin",
    export_value_name = short_name,
    required_concepts = c(
      "sex",
      "age",
      "CRP",
      "AGP"
    ),
    global_condition = age_in_years(age) >= 0, # no restrictions
    categories = list(
      category(
        name = "Adequate iron stores in apparently healthy individuals",
        !inflammation(CRP, AGP) &
          age_in_months(age) < 60 &
          value >= 12,

        !inflammation(CRP, AGP) &
          age_in_years(age) >= 5 &
          is_female(sex) &
          value >= 15 &
          value <= 150,

        !inflammation(CRP, AGP) &
          age_in_years(age) >= 5 &
          is_male(sex) &
          value >= 15 &
          value <= 200
      ),
      category(
        name = "Adequate iron stores in individuals with infection or inflammation",
        # children
        inflammation(CRP, AGP) &
          age_in_months(age) < 60 &
          value >= 30,

        inflammation(CRP, AGP) &
          age_in_years(age) >= 5 &
          value >= 70 &
          value <= 500
      ),
      category(
        name = "Iron deficiency in apparently healthy individuals",
        # children
        !inflammation(CRP, AGP) &
          age_in_months(age) < 60 &
          value < 12,

        !inflammation(CRP, AGP) &
          age_in_years(age) >= 5 &
          value < 15
      ),
      category(
        name = "Iron deficiency in individuals with infection or inflammation",
        # children
        inflammation(CRP, AGP) &
          age_in_months(age) < 60 &
          value < 30,

        inflammation(CRP, AGP) &
          #(is.na(is_pregnant(pregnancy_status)) | !is_pregnant(pregnancy_status)) &
          age_in_years(age) >= 5 &
          value < 70
      ),
      category(
        name = "Risk of overload in apparently healthy individuals",
        # female
        !inflammation(CRP, AGP) &
          is_female(sex) &
          #(is.na(is_pregnant(pregnancy_status)) | !is_pregnant(pregnancy_status)) &
          age_in_years(age) >= 5 &
          value > 150,

        !inflammation(CRP, AGP) &
          is_male(sex) &
          age_in_years(age) >= 5 &
          value > 200
      ),
      category(
        name = "Risk of overload in non-healthy individuals",
        inflammation(CRP, AGP) &
          is_female(sex) &
          #(is.na(is_pregnant(pregnancy_status)) | !is_pregnant(pregnancy_status)) &
          age_in_years(age) >= 5 &
          value > 500,

        inflammation(CRP, AGP) &
          is_male(sex) &
          age_in_years(age) >= 5 &
          value > 500
      )
    ),
    adjustment = value_adjustment,
    implausible_values = ferritin_implausible_adjustment(),
    prev_value_cutoffs = vec_c(
      lapply(c(5, 10, 12, 15, 30, 70, 100, 150), function(x) {
        new_prev_cutoff(
          new_function(pairlist2(value = ), bquote(value < .(x))),
          paste0(short_name, "_p", x)
        )
      }),
      lapply(c(200, 500), function(x) {
        new_prev_cutoff(
          new_function(pairlist2(value = ), bquote(value < .(x))),
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
                "Iron deficiency in apparently healthy individuals",
                "Iron deficiency in individuals with infection or inflammation"
              )
          )
        },
        \(x) {
          ifelse(
            is.na(x),
            NA,
            x %in%
              c(
                "Risk of overload in apparently healthy individuals",
                "Risk of overload in non-healthy individuals"
              )
          )
        }
      ),
      prevalence_category_names
    ),
    prevalence_category_names = c("Iron deficiency", "Risk of overload") |>
      stats::setNames(prevalence_category_names),
    drop_columns = list(
      short = NULL, #c("depletedironstores_unadj_se", "riskofironoverload_unadj_se"),
      long = NULL
    ),
    rename_columns = list(
      short = NULL,
      long = NULL
    ),
    reorder_columns = list(
      short = NULL,
      long = NULL
    )
  )
}

is_agp_implausible <- function(AGP) {
  #git424
  vals <- rep(FALSE, length(AGP))
  vals[is.na(AGP)] <- NA
  vals
  #!is.na(AGP) & (AGP < 0.5 | AGP > 8)
}

is_crp_implausible <- function(CRP) {
  #git424
  vals <- rep(FALSE, length(CRP))
  vals[is.na(CRP)] <- NA
  vals
  #!is.na(CRP) & (CRP < 0.1 | CRP > 4000)
}

is_ferritin_implausible <- function(ferritin) {
  !is.na(ferritin) & (ferritin < 0.5 | ferritin > 1000)
}

ferritin_is_implausible <- function(x, ...) {
  UseMethod("ferritin_is_implausible")
}

ferritin_implausible.default <- function(x, CRP, AGP) {
  ferritin_implausible_values(
    value = x,
    CRP = CRP,
    AGP = AGP
  )
}

ferritin_implausible_values <- function(value, CRP, AGP) {
  na <- measurement_mcg_l(NA_real_)

  value[is_ferritin_implausible(value)] <- na
  # value[is_agp_implausible(AGP)] <- na
  # value[is_crp_implausible(CRP)] <- na

  value
}

ferritin_adjustment_rm_agp_crp_fun <- function(
  value,
  CRP,
  AGP,
  malaria
) {
  if (length(CRP) > 0 && all(is.na(CRP))) {
    warning("CRP values are all NAs")
  }
  if (length(AGP) > 0 && all(is.na(AGP))) {
    warning("AGP values are all NAs")
  }
  if (length(malaria) > 0 && all(is.na(malaria))) {
    warning("malaria values are all NAs")
  }
  value <- vec_cast(value, double())
  na <- measurement_mcg_l(NA_real_)
  value[CRP >= 5] <- na
  value[AGP >= 1] <- na
  if (!is.null(malaria)) {
    value[malaria == "Malaria"] <- na
  }

  value
}

ferritin_implausible_adjustment <- function() {
  adjustment(
    required_concepts = NULL,
    fun = ferritin_implausible_values
  )
}

ferritin_adjustment_rm_agp_crp <- adjustment(
  required_concepts = c(
    "CRP",
    "AGP"
  ),
  fun = ferritin_adjustment_rm_agp_crp_fun,
  sub_class = "ferritin_adjustment_rm_agp_crp"
)


# This is not the same kind of adjustment. The "adjustment"
# happens in the inflammation function where it requires that
# this is the adjustment as part of "inflammation"
ferritin_adjustment_cutoff_fun <- function(value, CRP, AGP, malaria = NULL) {
  value <- vec_cast(value, double())
  value
}

ferritin_adjustment_cutoff <- adjustment(
  required_concepts = character(),
  fun = ferritin_adjustment_cutoff_fun,
  sub_class = "ferritin_adjustment_cutoff"
)

ferritin_adjustment_arithmetic_correction_fun <- function(value, CRP, AGP) {
  stopifnot(
    length(value) == length(CRP),
    length(value) == length(AGP)
  )
  if (length(CRP) > 0 && all(is.na(CRP))) {
    warning("CRP values are all NAs")
  }
  if (length(AGP) > 0 && all(is.na(AGP))) {
    warning("AGP values are all NAs")
  }

  # git296
  data <- tibble(
    iFerr = as.numeric(value),
    iCRP = as.numeric(CRP),
    iAGP = as.numeric(AGP)
  )

  data <- mutate(
    data,
    iCRPHigh = case_when(
      iCRP > 5 ~ 1, # Elevated CRP
      iCRP <= 5 ~ 0, # Normal CRP
      TRUE ~ NA_real_ # Assign NA to any other cases
    )
  )

  data <- mutate(
    data,
    iAGPHigh = case_when(
      iAGP > 1 ~ 1, # Elevated AGP
      iAGP <= 1 ~ 0, # Normal AGP
      TRUE ~ NA_real_ # Assign NA to any other cases
    )
  )

  data <- mutate(
    data,
    iInflamCat = case_when(
      iCRPHigh == 0 & iAGPHigh == 0 ~ 0, # None
      iCRPHigh == 1 & iAGPHigh == 0 ~ 1, # Incubation
      iCRPHigh == 1 & iAGPHigh == 1 ~ 2, # Early convalescent
      iCRPHigh == 0 & iAGPHigh == 1 ~ 3, # Late convalescent
      TRUE ~ NA_real_ # Assign NA to any other cases
    )
  )

  data$iLogFerr <- log(data$iFerr)

  data$iLogFerrCat0 <- mean(data$iLogFerr[data$iInflamCat == 0], na.rm = TRUE)
  data$iLogFerrCat1 <- mean(data$iLogFerr[data$iInflamCat == 1], na.rm = TRUE)
  data$iLogFerrCat2 <- mean(data$iLogFerr[data$iInflamCat == 2], na.rm = TRUE)
  data$iLogFerrCat3 <- mean(data$iLogFerr[data$iInflamCat == 3], na.rm = TRUE)

  data$iLogFerrCat0_1 <- data$iLogFerrCat0 - data$iLogFerrCat1
  data$iLogFerrCat0_2 <- data$iLogFerrCat0 - data$iLogFerrCat2
  data$iLogFerrCat0_3 <- data$iLogFerrCat0 - data$iLogFerrCat3

  data$iFerrAntilogCat1 <- exp(data$iLogFerrCat0_1)
  data$iFerrAntilogCat2 <- exp(data$iLogFerrCat0_2)
  data$iFerrAntilogCat3 <- exp(data$iLogFerrCat0_3)

  data <- mutate(
    data,
    iFerr2F4 = case_when(
      iInflamCat == 0 ~ iFerr, # None
      iInflamCat == 1 ~ iFerr * 0.53449, # Incubation
      iInflamCat == 2 ~ iFerr * 0.419884, # Early convalescent
      iInflamCat == 3 ~ iFerr * 0.8044426, # Late convalescent
      TRUE ~ NA_real_ # Assign NA to any other cases
    )
  )

  data$iFerr2F4
}

ferritin_adjustment_arithmetic_correction <- adjustment(
  required_concepts = c("CRP", "AGP"),
  fun = ferritin_adjustment_arithmetic_correction_fun,
  sub_class = "ferritin_adjustment_arithmetic_correction"
)

ferritin_adjustment_regression_correction <- adjustment(
  required_concepts = c("CRP", "AGP"),
  fun = function(value, CRP, AGP) {
    if (all(is.na(CRP)) && all(is.na(AGP))) {
      return(value)
    }

    CRP[CRP == 0] <- NA_real_
    AGP[AGP == 0] <- NA_real_
    value[value == 0] <- NA_real_

    data <- tibble(
      value = as.numeric(value),
      CRP = as.numeric(CRP),
      AGP = as.numeric(AGP),
      iLogFerr = log(value),
      iLogCRP = log(CRP),
      iLogAGP = log(AGP)
    )

    data$logcrpdecile <- quantile(data$iLogCRP, probs = 0.1, na.rm = TRUE)[[1]]
    data$logagpdecile <- quantile(data$iLogAGP, probs = 0.1, na.rm = TRUE)[[1]]
    valid_data <- !is.na(data$iLogFerr) & !is.na(data$iLogCRP) & !is.na(data$iLogAGP)
    original_length <- length(value)
    data <- data[valid_data, ]
    value <- value[valid_data]
    if (nrow(data) == 0) {
      # in this case the LM fit will error because we have 0 rows left with
      # non-na cases.
      stop(
        "All CRP, AGP or Ferritin values are either NA or 0. We cannot apply the regression correction."
      )
    }
    lmModel <- summary(lm(
      iLogFerr ~ iLogCRP + iLogAGP,
      data = data
    ))
    data$logcrpcoeffSF <- NA_real_
    data$logagpcoeffSF <- NA_real_
    if ("iLogCRP" %in% rownames(lmModel$coefficients)) {
      data$logcrpcoeffSF <- lmModel$coefficients["iLogCRP", "Estimate"]
    }
    if ("iLogAGP" %in% rownames(lmModel$coefficients)) {
      data$logagpcoeffSF <- lmModel$coefficients["iLogAGP", "Estimate"]
    }

    data <- mutate(
      data,
      iLogFerrAdj = case_when(
        iLogCRP > logcrpdecile & iLogAGP > logagpdecile ~ iLogFerr -
          logcrpcoeffSF * (iLogCRP - logcrpdecile) -
          logagpcoeffSF * (iLogAGP - logagpdecile),
        iLogCRP <= logcrpdecile & iLogAGP > logagpdecile ~ iLogFerr -
          logagpcoeffSF * (iLogAGP - logagpdecile),
        iLogCRP > logcrpdecile & iLogAGP <= logagpdecile ~ iLogFerr -
          logcrpcoeffSF * (iLogCRP - logcrpdecile),
        iLogCRP <= logcrpdecile & iLogAGP <= logagpdecile ~ iLogFerr,
        TRUE ~ NA_real_
      )
    )
    data$iFerr2F5 <- exp(data$iLogFerrAdj)
    result <- rep.int(NA_real_, length(original_length))
    if (
      is.data.frame(data) &&
        "iFerr2F5" %in% colnames(data)
    ) {
      stopifnot(nrow(data) == sum(valid_data, na.rm = TRUE))
      result[valid_data] <- data[["iFerr2F5"]]
      as.numeric(data[["iFerr2F5"]])
    }
    result
  },
  sub_class = "ferritin_adjustment_regression_correction"
)

is_ferritin_cutoff_adjustment <- function(x) {
  inherits(x, "ferritin_adjustment_cutoff")
}
is_ferritin_rm_agp_crp_adjustment <- function(x) {
  inherits(x, "ferritin_adjustment_rm_agp_crp")
}
is_ferritin_arithmetic_correction <- function(x) {
  inherits(x, "ferritin_adjustment_arithmetic_correction")
}
is_ferritin_regression_correction <- function(x) {
  inherits(x, "ferritin_adjustment_regression_correction")
}
