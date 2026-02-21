indicator <- function(
  name,
  abbreviated_name,
  required_concepts,
  global_condition,
  categories,
  value_concept,
  export_value_name,
  prev_value_cutoffs,
  prevalence_categories,
  aggregate_prevalence_categories = NULL,
  prevalence_category_names = NULL,
  adjustment = no_adjustment,
  implausible_values = no_implausible_values,
  drop_columns = NULL,
  rename_columns = NULL,
  reorder_columns = NULL,
  prevalence_reports = list(
    long = TRUE,
    short = TRUE
  ),
  precondition = TRUE,
  envir = caller_env()
) {
  stopifnot(
    all(
      vapply(prev_value_cutoffs, is_prev_cutoff, logical(1))
    )
  )

  structure(
    list(
      name = name,
      abbreviated_name = abbreviated_name,
      value_concept = value_concept,
      export_value_name = export_value_name,
      required_concepts = required_concepts,
      global_condition = enquo(global_condition),
      prev_value_cutoffs = prev_value_cutoffs,
      categories = categories,
      adjustment = adjustment,
      implausible_values = implausible_values,
      precondition = enquo(precondition),
      prevalence_categories = prevalence_categories,
      aggregate_prevalence_categories = aggregate_prevalence_categories,
      prevalence_category_names = prevalence_category_names,
      prevalence_reports = list(
        long = prevalence_reports$long %||% TRUE,
        short = prevalence_reports$short %||% TRUE
      ),
      drop_columns = drop_columns,
      rename_columns = rename_columns,
      reorder_columns = reorder_columns,
      parent_env = envir
    ),
    class = "indicator"
  )
}

new_prev_cutoff <- function(fun, name) {
  structure(list(fun = fun, name = name), class = "indicator_prev_cutoff")
}

is_prev_cutoff <- function(x) {
  inherits(x, "indicator_prev_cutoff")
}

indicator_apply_prev_cutoffs <- function(indicator, value) {
  stopifnot(is_indicator(indicator))
  cutoffs <- indicator$prev_value_cutoffs
  lapply(cutoffs, function(cutoff) {
    set_names(list(cutoff$fun(value)), cutoff$name)
  }) |>
    unlist(recursive = FALSE) |>
    as_tibble()
}

indicator_prev_cutoff_names <- function(indicator) {
  stopifnot(is_indicator(indicator))
  vapply(indicator$prev_value_cutoffs, \(x) x$name, character(1))
}

indicator_prevalence_categories <- function(indicator) {
  UseMethod("indicator_prevalence_categories")
}

#' @exportS3Method
indicator_prevalence_categories.indicator <- function(indicator) {
  indicator$prevalence_categories
}

indicator_agg_prevalence_categories <- function(indicator) {
  UseMethod("indicator_agg_prevalence_categories")
}

#' @exportS3Method
indicator_agg_prevalence_categories.indicator <- function(indicator) {
  indicator$aggregate_prevalence_categories
}

indicator_prevalence_names <- function(indicator) {
  stopifnot(is_indicator(indicator))
  indicator$prevalence_category_names
}

prevalence_report_long <- function(indicator) {
  isTRUE(indicator$prevalence_report$long)
}

prevalence_report_short <- function(indicator) {
  UseMethod("prevalence_report_short")
}

#' @exportS3Method
prevalence_report_short.indicator <- function(indicator) {
  isTRUE(indicator$prevalence_report$short)
}

indicator_export_value_name <- function(indicator) {
  UseMethod("indicator_export_value_name")
}

#' @exportS3Method
indicator_export_value_name.indicator <- function(indicator) {
  indicator$export_value_name
}

indicator_drop_columns <- function(indicator, report_type) {
  UseMethod("indicator_drop_columns")
}

#' @exportS3Method
indicator_drop_columns.indicator <- function(indicator, report_type) {
  indicator$drop_columns[[report_type]]
}

indicator_rename_columns <- function(indicator, report_type) {
  UseMethod("indicator_rename_columns")
}

#' @exportS3Method
indicator_rename_columns.indicator <- function(indicator, report_type) {
  indicator$rename_columns[[report_type]]
}

indicator_reorder_columns <- function(indicator, report_type) {
  UseMethod("indicator_reorder_columns")
}

#' @exportS3Method
indicator_reorder_columns.indicator <- function(indicator, report_type) {
  indicator$reorder_columns[[report_type]]
}

indicator_value_concept <- function(x) {
  UseMethod("indicator_value_concept")
}

#' @exportS3Method
indicator_value_concept.indicator <- function(x) {
  x$value_concept
}

is_indicator <- function(x) {
  inherits(x, "indicator")
}

indicator_name <- function(indicator) {
  UseMethod("indicator_name")
}

#' @exportS3Method
indicator_name.indicator <- function(indicator) {
  indicator$name
}

indicator_abbreviated_name <- function(indicator) {
  UseMethod("indicator_abbreviated_name")
}

#' @exportS3Method
indicator_abbreviated_name.indicator <- function(indicator) {
  indicator$abbreviated_name
}


indicator_adjustment <- function(indicator) {
  UseMethod("indicator_adjustment")
}

#' @exportS3Method
indicator_adjustment.indicator <- function(indicator) {
  indicator$adjustment
}

categories <- list

category <- function(name, ...) {
  dots <- rlang::enquos(...)
  structure(
    list(
      name = name,
      conditions = dots
    ),
    class = "indicator_category"
  )
}

#' @importFrom vctrs vec_c
adjustment <- function(required_concepts, fun, sub_class = NULL) {
  structure(
    list(
      required_concepts = required_concepts,
      fun = fun
    ),
    class = vec_c("indicator_adjustment", sub_class)
  )
}

is_adjustment <- function(x) {
  inherits(x, "indicator_adjustment")
}

required_concepts <- function(x) {
  UseMethod("required_concepts")
}

#' @exportS3Method
required_concepts.indicator <- function(x) {
  x$required_concepts
}

#' @exportS3Method
required_concepts.indicator_adjustment <- function(x) {
  x$required_concepts
}

no_adjustment <- adjustment(
  required_concepts = character(),
  fun = \(value) value
)

class(no_adjustment) <- c("indicator_no_adjustment", class(no_adjustment))
is_no_adjustment <- \(x) inherits(x, "indicator_no_adjustment")

is_cutoff_adjustment <- \(x) inherits(x, "ferritin_adjustment_cutoff")
is_adjustment_not_cutoff <- function(x) {
  !inherits(x, "indicator_no_adjustment") &
    !inherits(x, "ferritin_adjustment_cutoff")
}


no_implausible_values <- adjustment(
  required_concepts = character(),
  fun = \(value) value
)

#' @export
format.indicator <- function(x, ...) {
  x$name
}

#' @export
print.indicator <- function(x, ...) {
  cat(format(x), "\n")
}

indicator_compute <- function(x, value, concepts) {
  stopifnot(is_indicator(x))

  adj <- x$adjustment
  implausible_values <- x$implausible_values

  requirements <- c(
    required_concepts(x),
    required_concepts(adj),
    required_concepts(implausible_values)
  )

  required_concepts_missing <- !all(requirements %in% names(concepts))
  if (required_concepts_missing) {
    missing_concepts <- setdiff(requirements, names(concepts))
    stop(
      "Indicator: '",
      format(x),
      "' needs the following inputs: ",
      paste(missing_concepts, collapse = ", ")
    )
  }

  adjusted_value <- indicator_adjust_value(x, value, concepts)
  env_list <- c(
    list(value = adjusted_value),
    concepts
  )
  execution_envir <- list2env(env_list)
  parent.env(execution_envir) <- x$parent_env
  assign(
    "%between%",
    function(lhs, rhs) {
      !is.na(lhs) & lhs >= rhs[1] & lhs < rhs[2]
    },
    envir = execution_envir
  )
  cannot_compute <- !eval(rlang::get_expr(x$precondition), execution_envir)
  if (cannot_compute) {
    stop(
      "Indicator: '",
      format(x),
      "' cannot be computed because a precondition is not met.",
      "Precondition: ",
      x$precondition
    )
    return(NULL)
  }
  conditions <- unlist(
    lapply(x$categories, function(x) {
      category_conditions_to_formula_list(x, execution_envir)
    }),
    recursive = FALSE,
    use.names = TRUE
  )

  result <- dplyr::case_when(
    !!!conditions,
    TRUE ~ NA_character_
  )
  global_condition <- rlang::get_expr(x$global_condition)
  result[
    !na_2_false(eval(global_condition, envir = execution_envir))
  ] <- NA_character_
  factor(
    x = result,
    levels = levels(x)
  )
}

indicator_adjust_value <- function(indicator, value, concepts) {
  stopifnot(is_indicator(indicator))

  # These are the adjustment and implausible values FUNCTIONS (not values)
  adj <- indicator$adjustment
  implausible_values <- indicator$implausible_values

  # Apply the adjustment
  if (
    "ferritin_adjustment_rm_agp_crp" %in%
      class(adj) &
      "malaria" %in% names(concepts)
  ) {
    adj$required_concepts <- c(adj$required_concepts, "malaria")
  }

  adjusted_value <- do.call(
    adj$fun,
    c(
      list(value),
      concepts[required_concepts(adj)]
    )
  )

  # Set implausible values to NA
  do.call(
    implausible_values$fun,
    c(list(adjusted_value), concepts[required_concepts(implausible_values)])
  )
}

indicators_compute_all <- function(indicators, values, concepts) {
  # ATM we have two types of indicators. Normal indicators and composite indicators.
  # Composite indicators depend on non-composite indicators and are computed at the
  # end.
  base_indicators <- Filter(is_indicator, indicators)
  composite_indicators <- Filter(is_composite_indicator, indicators)
  without_null <- \(v) Filter(\(x) !is.null(x), v)
  indicator_values <- lapply(seq_along(base_indicators), function(i) {
    indicator <- base_indicators[[i]]

    res <- if (is.null(values[[i]])) {
      stop(
        "Indicator: '",
        format(indicator),
        "'. Please provide a value for '",
        indicator$value_concept,
        "'",
        call. = FALSE
      )
    } else {
      result <- indicator_compute(indicator, values[[i]], concepts)
      if (is.null(result)) {
        tibble::tibble(
          result = NA_real_,
          input_value = NA_real_
        )
      } else {
        tibble::tibble(
          result = result,
          input_value = indicator_adjust_value(indicator, values[[i]], concepts)
        )
      }
    }
    has_adjustment <- !is_no_adjustment(indicator_adjustment(indicator))
    if (has_adjustment) {
      res$adjustment_method <- adjustment_name(indicator_adjustment(indicator))
    }
    colnames(res) <- paste0(
      indicator_abbreviated_name(indicator),
      "_",
      colnames(res)
    )
    res
  })
  indicator_names <- vapply(
    base_indicators,
    indicator_abbreviated_name,
    character(1)
  )
  iv <- set_names(indicator_values, indicator_names)
  # now we compute the composite indicators
  # first we check dependencies
  for (indicator in composite_indicators) {
    deps <- indicator$required_indicators
    if (!all(deps %in% indicator_names)) {
      missing_deps <- setdiff(deps, indicator_names)
      missing_deps <- indicator_map[missing_deps]
      stop(
        "Composite indicator '",
        format(indicator),
        "' requires the following other indicators: ",
        paste(
          paste0(
            "'",
            vapply(missing_deps, format, character(1L)),
            "'"
          ),
          collapse = ", "
        ),
        call. = FALSE
      )
    }
  }
  # now we can compute everything
  iv <- Reduce(
    function(acc, x) {
      x$compute_function(x, indicators, acc)
    },
    composite_indicators,
    iv
  )
  iv
}

na_2_false <- function(x) {
  stopifnot(is.logical(x))
  x[is.na(x)] <- FALSE
  x
}

category_conditions_to_formula_list <- function(cat, envir) {
  lapply(cat$conditions, function(x) {
    rlang::new_formula(
      lhs = rlang::get_expr(rlang::quo(na_2_false(!!rlang::get_expr(x)))),
      rhs = cat$name,
      env = envir
    )
  })
}

#' @export
levels.indicator <- function(x) {
  vapply(
    x$categories,
    function(category) {
      category$name
    },
    character(1)
  )
}

age_in_months <- function(age) {
  stopifnot(is_age(age))
  age_as_numeric(age, "months")
}

age_in_years <- function(age) {
  stopifnot(is_age(age))
  age_as_numeric(age, "years")
}

flatten_indicators <- function(indicators) {
  result <- list()
  for (el in indicators) {
    is_single_indicator <- is_indicator(el) || is_composite_indicator(el)
    if (!is_single_indicator && is.list(el)) {
      result <- append(result, el)
    } else if (is_single_indicator) {
      result[[length(result) + 1L]] <- el
    } else {
      stop("Indicators need to be either indicators or list of indicators.")
    }
  }
  result
}
