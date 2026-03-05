#' Individual Classification
#'
#' Provides individual classifications of micronutrient status according to
#' WHO recommended cutoffs to define micronutrient status of populations.
#'
#' @param indicators a list of indicators that should be computed. Note, please do not add the same indicator twice. Also you can only use one adjustment at a time at the moment.
#' @param sex A vector indicating the sex of individuals. Accepted values: for Male (1/ "M"/ "m") and for Female (2/ "F"/ "f").
#' .           If missing, prevalence will not be calculated for any biomarker because micronutrient status cut offs are sex-specific.
#' @param age A numeric vector of ages in years for the individuals in the dataset. If you have age in other units, you have to use the \code{age} object. For example: if you have age in months, you can create an `age` object like this \code{age(age_in_months, unit = "months")}.
#' @param pregnancy_status (Optional) A vector indicating pregnancy status. Accepted values: For Yes ("Y", "y", or "1"), No ("N", "n", or "2"), Unknown ("unk" or "3" or blank).
#'                         When Unknown ("unk" or "3" or blank), it will be categorized as "not pregnant".
#'                         Note that 'pregnancy_status' should not be missing when 'pregnancyweeks' or 'pregnancymonths' contain valid, non-missing values. Observations with missing values in 'pregnancy_status' will be considered as 'not pregnant'.
#' @param lactating_status (Optional) A vector indicating lactation status. Accepted values: For Yes ("Y", "y", or "1"), No ("N", "n", or "2").
#' @param pregnancyweeks (Optional) A numeric vector indicating the number of weeks of pregnancy.
#' @param pregnancymonths (Optional) A numeric vector indicating the number of months of pregnancy.
#' @param CRP (Optional) A vector of C-reactive protein (CRP) measurements (in mg/L), used to adjust for inflammation.
#' @param AGP (Optional) A vector of alpha-1-acid glycoprotein (AGP) measurements (in g/L), used to adjust for inflammation.
#' @param ferritin (Optional) A vector of ferritin measurements (in \\u00b5gg/L).
#' @param iodine (Optional) A vector of iodine measurements (in \\u00b5gg/L).
#' @param haemoglobin (Optional) A vector of haemoglobin measurements (in g/L).
#' @param altitude (Optional) A numeric vector representing elevation above sea level (in meters), used to adjust for altitude-related effects. Elevation is a compulsory variable and it should always be reported in the dataset. Even when no elevation data is collected, a variable for 'elevation' should be created and set as "0" for all individuals without reported elevation. When elevation is not reported, that individual case will be excluded from the analysis and considered as 'missing'
#' @param is_smoker (Optional) A vector indicating smoking status. Accepted values: for Yes ("Y", "y", or "1"), No ("N", "n", or "2"), Unknown ("unk" or "3"). Note that 'is_smoker' should not be missing when 'smokes_cigarettes_per_day' contains valid and not missing values. Observations with missing values in 'is_smoker' will be considered as a 'non-smoker'.
#' @param smokes_cigarettes_per_day (Optional) A numeric vector representing the number of cigarettes smoked per day. Note that 'is_smoker' should not be missing when 'smokes_cigarettes_per_day' contain valid, non-missing values. Missing 'is_smoker' in these cases will be considered as 'not a smoker'.
#' @param malaria (Optional) A vector indicating malaria status. Accepted values: for Yes (“Y”, “y”, or “1”) and No (“N”, “n”, or “2”).
#'
#' @return A data frame with classification results for each individual.
#' For each indicator the result of the indicator and its adjusted input value is included.
#' Also each input for the computation is also listed.
#'
#' @examples
#' \dontrun{
#' data <- read.csv(...)
#'
#' # Classify the dataset
#' individual_classification(
#'  indicators = list(
#'    indicator_iodine(),
#'    indicator_ferritin(adjustment_ferritin_cutoff()),
#'    indicator_anaemia(),
#'    indicator_ida()
#'  ),
#'  age = data$age_years,
#'  sex = data$sex,
#'  pregnancy_status = data$pregnancy_status,
#'  lactating_status = data$lactating_status,
#'  ferritin = data$ferritin_measurement,
#'  iodine = data$iodine,
#'  CRP = data$crp_measurement,
#'  AGP = data$agp_measurement,
#'  haemoglobin = data$haemoglobin_measurement,
#'  is_smoker = data$is_smoker,
#'  altitude = data$altitude,
#'  smokes_cigarettes_per_day = data$smokes_cigarettes_per_day,
#'  pregnancyweeks = data$pregnancyweeks,
#'  pregnancymonths = data$pregnancymonths
#')
#'
#' # View results
#' print(res)
#' }
#' @export
individual_classification <- function(
  indicators,
  sex,
  age,
  pregnancy_status = NULL,
  pregnancyweeks = NULL,
  pregnancymonths = NULL,
  lactating_status = NULL,
  CRP = NULL,
  AGP = NULL,
  ferritin = NULL,
  iodine = NULL,
  haemoglobin = NULL,
  altitude = NULL,
  is_smoker = NULL,
  smokes_cigarettes_per_day = NULL,
  malaria = NULL
) {
  classify_data_internal(
    indicators,
    sex = sex,
    age = age,
    pregnancy_status = pregnancy_status,
    lactating_status = lactating_status,
    CRP = CRP,
    AGP = AGP,
    iodine = iodine,
    ferritin = ferritin,
    haemoglobin = haemoglobin,
    altitude = altitude,
    is_smoker = is_smoker,
    smokes_cigarettes_per_day = smokes_cigarettes_per_day,
    pregnancyweeks = pregnancyweeks,
    pregnancymonths = pregnancymonths,
    malaria = malaria,
    .format_column_names = TRUE
  )
}

classify_data_internal <- function(
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
  .format_column_names = TRUE
) {
  indicators <- flatten_indicators(indicators)
  validate_indicators(indicators)
  concept_list <- concepts_from_args(
    age = age,
    sex = sex,
    pregnancy_status = pregnancy_status,
    pregnancyweeks = pregnancyweeks,
    pregnancymonths = pregnancymonths,
    lactating_status = lactating_status,
    is_smoker = is_smoker,
    smokes_cigarettes_per_day = smokes_cigarettes_per_day,
    altitude = altitude,
    iodine = iodine,
    haemoglobin = haemoglobin,
    ferritin = ferritin,
    CRP = CRP,
    AGP = AGP,
    malaria = malaria
  )
  cols <- names(concept_list$values)
  validate_concepts(concept_list)
  values <- lapply(indicators, function(x) {
    value_concept <- indicator_value_concept(x)
    if (is.null(value_concept)) {
      NULL
    } else {
      concept_list$values[[value_concept]]
    }
  })
  results <- indicators_compute_all(indicators, values, concept_list$values)
  names(results) <- NULL
  df <- do.call(cbind, results)
  if (!.format_column_names) {
    return(df)
  }
  colnames(df) <- paste0("indicator_", colnames(df))
  concept_df <- dplyr::bind_cols(concept_list$values[concept_list$non_nulls])
  # age is part of the output in years and months
  concept_df$age_years <- age_as_numeric(concept_df$age, "years")
  concept_df$age_months <- age_as_numeric(concept_df$age, "months")
  concept_df$age <- NULL
  concept_df <- concept_df[, c(
    c("age_years", "age_months"),
    setdiff(colnames(concept_df), c("age_years", "age_months"))
  )]
  colnames(concept_df) <- paste0("input_", colnames(concept_df))
  dplyr::bind_cols(concept_df, df)
}

# validate_concepts does some general prechecks independent of
# the actual indicators being used.
validate_concepts <- function(concepts) {
  is_smoker <- concepts$values$is_smoker
  smokes_cigarettes_per_day <- concepts$values$smokes_cigarettes_per_day
  if (!is.null(is_smoker) && !is.null(smokes_cigarettes_per_day)) {
    if (any(is.na(is_smoker) & !is.na(smokes_cigarettes_per_day))) {
      warning(
        "Missing `is_smoker`: a non NA value for `is_smoker` is required when `smokes_cigarettes_per_day` is not NA. Otherwise, these observations will be considered as 'not smoker'"
      )
    }
  }
  pregnancy_status <- concepts$values$pregnancy_status
  pregnancyweeks <- concepts$values$pregnancyweeks
  pregnancymonths <- concepts$values$pregnancymonths
  if (!is.null(pregnancy_status) && !is.null(pregnancyweeks)) {
    if (any(is.na(pregnancy_status) & !is.na(pregnancyweeks))) {
      warning(
        "Missing `pregnancy_status`: a non NA value for `pregnancy_status` is required when `pregnancyweeks` is not NA. Otherwise, these observations will be considered as 'not pregnant'."
      )
    }
  }
  if (!is.null(pregnancy_status) && !is.null(pregnancymonths)) {
    if (any(is.na(pregnancy_status) & !is.na(pregnancymonths))) {
      warning(
        "Missing `pregnancy_status`: a non NA value for `pregnancy_status` is required when `pregnancymonths` is not NA. Otherwise, these observations will be considered as 'not pregnant'."
      )
    }
  }
}

validate_indicators <- function(indicators) {
  stopifnot(is.list(indicators))
  stopifnot(all(vapply(
    indicators,
    function(x) {
      is_indicator(x) || is_composite_indicator(x)
    },
    logical(1L)
  )))
}

not_null <- function(x) {
  !is.null(x)
}
