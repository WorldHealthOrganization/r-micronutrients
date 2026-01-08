#' Each variable that can be mapped by a user is represented internally as a
#' 'concept'.
#'
#' @include concept-sex.R
#' @include concept-wealth-quintile.R
#' @include concept-mothers-education.R
#' @include concept-area.R
#' @include measurements.R
#' @include indicators-iodine.R
#' @include indicators-anaemia.R
#' @include indicators-ferritin.R
#' @include concept-helpers.R
#'
#' @noRd
concepts <- concepts_list(
  ## age ----
  concept(
    key = "age",
    label = "Age",
    acceptor = concept_acceptor(is.numeric, "Age needs to be a numeric"),
    standardizer = identity,
    validator = lubridate::is.duration,
    prototype = lubridate::duration(NA_real_)
  ),
  ## sex ----
  concept(
    key = "sex",
    label = "Sex",
    acceptor = concept_acceptor(
      sex_acceptor,
      "Sex can be f,m,2,1,Female,Male,female,male"
    ),
    standardizer = sex_standardizer,
    validator = \(x) is.factor(x) && all(levels(x) == sex_levels),
    prototype = factor(NA_character_, levels = sex_levels)
  ),
  ## pregnancy_status ----
  concept(
    key = "pregnancy_status",
    label = "Pregnacy status",
    acceptor = concept_acceptor(
      function(x) {
        x <- as.character(x)
        all(
          tolower(x) %in%
            c(
              "1",
              "2",
              "3",
              "y",
              "n",
              "unk",
              "unknown",
              "yes",
              "no",
              NA_character_
            )
        ) &&
          any(!is.na(x))
      },
      "Pregancy status is encoded as either 1 (yes), 2 (no) or 3 (unknown)."
    ),
    standardizer = function(x) {
      x <- as.character(x)
      res <- rep.int(NA_character_, length(x))
      res[x %in% c("1", "y", "yes")] <- "Pregnant"
      res[x %in% c("2", "n", "no", "unk", "unknown")] <- "Not Pregnant"
      stopifnot(all(res %in% c(pregnancy_status_levels, NA_character_)))
      factor(res, levels = pregnancy_status_levels)
    },
    validator = \(x) is.factor(x) && all(levels(x) == pregnancy_status_levels),
    prototype = NA_character_
  ),
  ## prenancymonths ----
  concept(
    key = "pregnancymonths",
    label = "Pregnancy Months",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = NA_real_
  ),
  ## pregnancyweeks ----
  concept(
    key = "pregnancyweeks",
    label = "Pregnancy Weeks",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = NA_real_
  ),
  ## lactating_status ----
  concept(
    key = "lactating_status",
    label = "Lactating status",
    acceptor = concept_acceptor(
      function(x) {
        x <- as.character(x)
        all(x %in% c("1", "2", "3", NA_character_)) && any(!is.na(x))
      },
      "Lactating status is encoded as either 1 (yes), 2 (no) or 3 (unknown)."
    ),
    standardizer = function(x) {
      x <- as.character(x)
      res <- rep.int(NA, length(x))
      res[x == "1"] <- "Lactating"
      res[x == "2"] <- "Not Lactating"
      stopifnot(all(res %in% c(lactating_status_levels, NA_character_)))
      factor(res, levels = lactating_status_levels)
    },
    validator = \(x) is.factor(x) && all(levels(x) == lactating_status_levels),
    prototype = NA
  ),
  ## is_smoker ----
  concept(
    key = "is_smoker",
    label = "Smoking Status",
    acceptor = concept_acceptor(
      function(x) {
        x <- as.character(x)
        all(
          tolower(x) %in%
            c(
              "true",
              "false",
              "1",
              "2",
              "3",
              "y",
              "n",
              "unk",
              "unknown",
              "yes",
              "no",
              NA_character_
            )
        ) &&
          any(!is.na(x))
      },
      "Smoking status is encoded as 1 (yes) or 2 (no)."
    ),
    standardizer = function(x) {
      x <- as.character(x)
      res <- rep.int(NA, length(x))
      res[x == "1"] <- TRUE
      res[x == "2"] <- FALSE
      res[x == "3"] <- FALSE
      res[tolower(x) == "yes"] <- TRUE
      res[tolower(x) == "no"] <- FALSE
      res[tolower(x) == "true"] <- TRUE
      res[tolower(x) == "false"] <- FALSE
      res[x == "unk"] <- FALSE
      res[x == "unknown"] <- FALSE

      res
    },
    validator = is.logical,
    prototype = NA
  ),
  ## smokes_packets_per_day ----
  concept(
    key = "smokes_cigarettes_per_day",
    label = "Number of cigarettes per day",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = NA_real_
  ),
  ## altitude ----
  concept(
    key = "altitude",
    label = "Elevation",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = NA_real_
  ),
  ## area ----
  concept(
    key = "area",
    label = "Area",
    acceptor = concept_acceptor(
      function(x) {
        all(
          tolower(x) %in% c("1", "2", "urban", "rural", NA_character_),
          na.rm = TRUE
        ) &&
          any(!is.na(x))
      },
      "TODO"
    ),
    standardizer = as_area,
    validator = \(x) is.factor(x) & all(levels(x) == area_levels),
    prototype = NA_character_
  ),
  ## region ----
  concept(
    key = "region",
    label = "Region",
    acceptor = concept_acceptor(\(x) is.character(x) | is.numeric(x), "TODO"),
    standardizer = as.character,
    validator = is.character,
    prototype = NA_character_
  ),
  ## wealth_quintile ----
  concept(
    key = "wealth_quintile",
    label = "Wealth Quintile",
    acceptor = concept_acceptor(wealth_quintiles_acceptor, "TODO"),
    standardizer = as_wealth_quintiles,
    validator = \(x) is.factor(x) & all(levels(x) == wealth_quintiles_levels),
    prototype = NA_character_
  ),
  ## mothers_education ----
  concept(
    key = "mothers_education",
    label = "Mother's Education",
    acceptor = concept_acceptor(
      function(x) {
        is.numeric(x) &&
          all(
            x %in% c(0:3, NA_integer_),
            na.rm = TRUE
          ) &&
          any(!is.na(x))
      },
      "TODO"
    ),
    standardizer = as_mothers_education,
    validator = \(x) is.factor(x) & all(levels(x) == mothers_education_levels),
    prototype = NA_character_
  ),
  ## sample_weight ----
  concept(
    key = "sample_weight",
    label = "Sample Weight",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = NA_real_
  ),
  ## iodine ----
  concept(
    key = "iodine",
    label = "Urinary Iodine Concentration (\u00b5g/L)",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = measurement_g_l(NA_real_),
    is_implausible = is_iodine_implausible
  ),
  ## haemoglobin ----
  concept(
    key = "haemoglobin",
    label = "Haemoglobin (g/L)",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = measurement_g_l(NA_real_),
    is_implausible = is_implausible_anaemia
  ),
  ## ferritin ----
  concept(
    key = "ferritin",
    label = "Ferritin (\u00b5g/L)",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = measurement_mcg_l(NA_real_),
    is_implausible = is_ferritin_implausible
  ),
  ## AGP ----
  concept(
    key = "AGP",
    label = "AGP",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = measurement_g_l(NA_real_),
    is_implausible = is_agp_implausible
  ),
  ## CRP ----
  concept(
    key = "CRP",
    label = "CRP",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = measurement_mg_l(NA_real_),
    is_implausible = is_crp_implausible
  ),
  ## other_grouping_variable ----
  concept(
    key = "other_grouping_variable",
    label = "Other filter",
    acceptor = concept_acceptor(\(x) is.character(x) | is.numeric(x), "TODO"),
    standardizer = as.character,
    validator = is.character,
    prototype = NA_character_
  ),
  ## cluster ----
  concept(
    key = "cluster",
    label = "Cluster",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = NA_integer_
  ),
  ## strata ----
  concept(
    key = "strata",
    label = "Strata",
    acceptor = concept_acceptor(is.numeric, "TODO"),
    standardizer = identity,
    validator = is.numeric,
    prototype = NA_integer_
  ),
  ## fasting ----
  concept(
    key = "fasting",
    label = "Fasting",
    acceptor = concept_acceptor(
      function(x) {
        all(
          tolower(x) %in% c("1", "2", "yes", "no", "y", "n", NA_character_),
          na.rm = TRUE
        ) &&
          any(!is.na(x))
      },
      "TODO"
    ),
    standardizer = as.character,
    validator = is.character,
    prototype = NA_character_
  ),
  ## malaria ----
  concept(
    key = "malaria",
    label = "Malaria",
    acceptor = concept_acceptor(
      function(x) {
        all(
          tolower(x) %in% c("1", "2", "yes", "no", "y", "n", NA_character_),
          na.rm = TRUE
        ) &&
          any(!is.na(x))
      },
      "TODO"
    ),
    standardizer = function(x) {
      x <- as.character(x)
      res <- rep.int(NA, length(x))
      res[x == "1"] <- "Malaria"
      res[x == "2"] <- "No Malaria"
      res[x == "yes"] <- "Malaria"
      res[x == "no"] <- "No Malaria"
      res[x == "y"] <- "Malaria"
      res[x == "n"] <- "No Malaria"
      factor(res, levels = c("Malaria", "No Malaria"))
    },
    validator = is.factor,
    prototype = NA_character_
  )
)

#' optional_concepts is a list of all concepts that do not need to be supplied
#' by the user. In case the user does not explicitly assigns values to these
#' the NA values of the specific concept is used.
#' @noRd
optional_concepts <- c(
  "is_smoker",
  "smokes_cigarettes_per_day",
  "pregnancy_status",
  "pregnancyweeks",
  "pregnancymonths",
  "lactating_status",
  "CRP",
  "AGP",
  "malaria"
)

#' concepts_from_args takes a variadic pairlist
#' and makes it a named list with non-null values.
#'
#' @return a list with two componments: `values` contains the named list
#' of all values. `non_nulls` contains a character vector that indicates
#' which concepts have been explicitly defined because some concepts
#' create default values in case they have not been defined explicitly.
#'
#' @noRd
concepts_from_args <- function(...) {
  all_values <- list(...)
  vals <- Filter(not_null, all_values)
  keys <- names(vals)
  if (length(vals) == 0) {
    return(list(
      values = list(),
      non_nulls = character()
    ))
  }
  # at last we check that all values have the same length
  ll <- lengths(vals)
  len <- ll[[1]]
  all_equal_length <- all(vapply(ll, "==", logical(1L), len))
  if (!all_equal_length) {
    stop("All values need to be of equal length")
  }
  vals <- lapply(seq_along(vals), function(i) {
    key <- keys[[i]]
    concept <- concepts[[key]]
    vals <- vals[[i]]
    if (!concept$acceptor$fun(vals)) {
      stop("Invalid value for ", key, ": ", concept$acceptor$error_msg)
    }
    if (key == "age") {
      # age is special
      if (!lubridate::is.duration(vals)) {
        vals <- lubridate::duration(vals, "years")
      }
      return(concept$standardizer(vals))
    }
    concept$standardizer(vals)
  })
  names(vals) <- keys
  value_length <- length(vals[[1]])
  for (optional_concept_key in optional_concepts) {
    if (optional_concept_key %in% keys) {
      next()
    }
    optional_concept <- concepts[[optional_concept_key]]
    if (is.null(optional_concept)) {
      stop("Internal error: optional concept is null")
    }
    vals <- c(
      vals,
      stats::setNames(
        list(rep.int(optional_concept$prototype, value_length)),
        optional_concept_key
      )
    )
  }
  list(
    values = vals,
    non_nulls = keys
  )
}

#' A helper data structure to map concept keys to labels
#' here for legacy reasons.
#' @noRd
concept_label <- set_names(
  vapply(concepts, \(x) x$label, character(1)),
  concept_keys(concepts)
)
