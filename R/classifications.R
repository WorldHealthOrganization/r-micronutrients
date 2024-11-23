#' Individual Classification
#'
#' Provides individual classifications of micronutrient status according to
#' WHO recommended cutoffs to define micronutrient status of populations.
#'
#' @param indicators a list of indicators that should be computed. Note, please do not add the same indicator twice.
#' @param sex A vector indicating the sex of individuals. Accepted values: for Male (1/ "M"/ "m") and for Female (2/ "F"/ "f").
#' .           If missing, prevalence will not be calculated for any biomarker because micronutrient status cut offs are sex-specific.
#' @param age A vector of ages in years for the individuals in the dataset. Can also be a \code{lubridate::duration} object.
#' @param pregnancy_status (Optional) A vector indicating pregnancy status. Accepted values: For Yes ("Y", "y", or "1"), No ("N", "n", or "2"), Unknown ("unk" or "3" or blank).
#'                         When Unknown ("unk" or "3" or blank), it will be categorized as "not pregnant"
#' @param lactating_status (Optional) A vector indicating lactation status. Accepted values: For Yes ("Y", "y", or "1"), No ("N", "n", or "2").
#' @param pregnancyweeks (Optional) A numeric vector indicating the number of weeks of pregnancy.
#' @param pregnancymonths (Optional) A numeric vector indicating the number of months of pregnancy.
#' @param CRP (Optional) A vector of C-reactive protein (CRP) measurements (in mg/L), used to adjust for inflammation.
#' @param AGP (Optional) A vector of alpha-1-acid glycoprotein (AGP) measurements (in g/L), used to adjust for inflammation.
#' @param ferritin (Optional) A vector of ferritin measurements (in \\u00b5gg/L).
#' @param iodine (Optional) A vector of iodine measurements (in \\u00b5gg/L).
#' @param haemoglobin (Optional) A vector of haemoglobin measurements (in g/L).
#' @param altitude (Optional) A numeric vector representing elevation above sea level (in meters), used to adjust for altitude-related effects. Elevation is a compulsory variable and it should always be reported in the dataset. Even when no elevation data is collected, a variable for 'elevation' should be created and set as "0" for all individuals without reported elevation. When elevation is not reported, that individual case will be excluded from the analysis and considered as 'missing'
#' @param is_smoker (Optional) A vector indicating smoking status. Accepted values: for Yes ("Y", "y", or "1"), No ("N", "n", or "2"), Unknown ("unk" or "3"). When ‘smoking status’ is mapped and no value is reported, the tool will consider this value as "no smoking".
#' @param smokes_cigarettes_per_day (Optional) A numeric vector representing the number of cigarettes smoked per day.
#' @param malaria (Optional) A vector indicating malaria status. Accepted values: for Yes (“Y”, “y”, or “1”) and No (“N”, “n”, or “2”).
#'
#' @return A data frame with classification results for each individual.
#' For each indicator the result of the indicator and its adjusted input value is included.
#' Also each input for the computation is also listed.
#'
#' @examples
#' \dontrun{
#' data <- reade.csv(...)
#'
#' # Classify the dataset
#' classify_data(
#'   indicators = list(iodine_indicator, ferritin_indicator(no_adjustment)),
#'   age = data$age_years,
#'   sex = data$sex,
#'   pregnancy_status = data$pregnancy_status,
#'   lactating_status = data$lactating_status,
#'   ferritin = data$ferritin_measurement,
#'   CRP = data$crp_measurement,
#'   AGP = data$agp_measurement,
#'   haemoglobin = data$haemoglobin_measurement
#' )
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
    malaria = NULL) {
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
    .format_column_names = TRUE) {
  validate_indicators(indicators)
  concept_list <- concepts_from_args(
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
    malaria = malaria
  )
  cols <- names(concept_list)

  values <- lapply(indicators, function(x) {
    concept_list[[x$value_concept]]
  })

  results <- indicators_compute_all(indicators, values, concept_list)
  names(results) <- NULL
  df <- do.call(cbind, results)
  if (!.format_column_names) {
    return(df)
  }
  colnames(df) <- paste0("indicator_", colnames(df))
  concept_df <- do.call(cbind, concept_list)
  colnames(concept_df) <- paste0("input_", colnames(concept_df))
  cbind(concept_df, df)
}

validate_indicators <- function(indicators) {
  stopifnot(is.list(indicators))
  stopifnot(all(vapply(indicators, is_indicator, logical(1L))))
}

not_null <- function(x) {
  !is.null(x)
}
