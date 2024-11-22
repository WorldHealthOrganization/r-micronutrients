#' Classify Survey Data Based on Micronutrient Indicators
#'
#' This function classifies survey data by calculating micronutrient status based on provided measurements
#' such as ferritin, haemoglobin, and inflammation markers (CRP and AGP). It uses predefined global indicators
#' to determine classification results for various micronutrient deficiencies and adequacy, while accounting for
#' additional contextual factors such as pregnancy, smoking, altitude, and malaria status.
#'
#' @param sex A vector indicating the sex of individuals. Valid values are "m", "1", "Male" for males and "f", "2", "Female" for females.
#' @param age A vector of ages in years for the individuals in the dataset. Can also be a \code{lubridate::duration} object.
#' @param pregnancy_status (Optional) A vector indicating pregnancy status. Encoded as either 1 (yes), 2 (no), or 3 (unknown).
#' @param lactating_status (Optional) A vector indicating lactation status. Encoded as either 1 (yes), 2 (no), or 3 (unknown).
#' @param CRP (Optional) A vector of C-reactive protein (CRP) measurements (in mg/L), used to adjust for inflammation.
#' @param AGP (Optional) A vector of alpha-1-acid glycoprotein (AGP) measurements (in g/L), used to adjust for inflammation.
#' @param ferritin (Optional) A vector of ferritin measurements (in \\u00b5gg/L).
#' @param iodine (Optional) A vector of iodine measurements (in \\u00b5gg/L).
#' @param haemoglobin (Optional) A vector of haemoglobin measurements (in g/L).
#' @param altitude (Optional) A numeric vector representing elevation above sea level (in meters), used to adjust for altitude-related effects.
#' @param is_smoker (Optional) A vector indicating smoking status. Valid values are 1 (yes), 2 (no), or equivalent labels like "yes", "no".
#' @param smokes_cigarettes_per_day (Optional) A numeric vector representing the number of cigarettes smoked per day.
#' @param pregnancyweeks (Optional) A numeric vector indicating the number of weeks of pregnancy.
#' @param pregnancymonths (Optional) A numeric vector indicating the number of months of pregnancy.
#' @param malaria (Optional) A vector indicating malaria status. Encoded as 1 (yes), 2 (no), or equivalent labels like "yes", "no".
#'
#' @return A data frame with classification results for each individual. The output includes:
#' - `*_result`: The classification result for each indicator (e.g., "Adequate iron stores", "Iron deficiency").
#' - `*_input_value`: The input values corresponding to each classification (e.g., ferritin levels, haemoglobin levels).
#'
#' @details
#' The indicators used include:
#' - Iodine deficiency (short: iodine)
#' - Anaemia (short: anemia)
#' - Iron-deficiency anaemia (IDA) with no adjustment (short: ida)
#' - Ferritin with inflammation adjustment (short: ferritin_adj)
#' - Ferritin without adjustment (short: ferritin_unadj)
#'
#' @examples
#' \dontrun{
#' data <- reade.csv(...)
#'
#' # Classify the dataset
#' classify_data(
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
#' @include indicators-all.R
#' @export
classify_data <- function(
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
    malaria = NULL) {
  classify_data_internal(
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

  values <- lapply(global_indicators, function(x) {
    concept_list[[x$value_concept]]
  })

  results <- indicators_compute_all(global_indicators, values, concept_list)
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

not_null <- function(x) {
  !is.null(x)
}
