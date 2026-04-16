#' Load and Process Water Quality Data
#'
#' This function loads water quality data from a cleaned CSV file and performs
#' comprehensive data processing including column renaming, parameter mapping,
#' and filtering for valid geographic coordinates and chlorophyll values.
#'
#' @param fpath Character string specifying the file path to the cleaned CSV data file.
#'              Typically uses "data/dashboardDataSEACAR.csv" for the dashboard.
#'
#' @return A data frame containing processed water quality data

load_wq_cleaned_df <- function(fpath){
  # load data from cleaned df csv file.
  epcdata <- readr::read_csv(
    fpath
  ) |>
    dplyr::mutate(
      Site = ProgramLocationID,
      bay_segment = ProgramName,
      Source = ProgramName,
      Parameter = ParameterName,
      Units = ParameterUnits,
      Sample.Depth = `ActivityDepth_m`,
      SampleTimeString = as.character(SampleDate),
      datetime = as.Date(SampleDate),
      SampleTime = SampleDate,
      yr = year(SampleDate),
      mo = month(SampleDate),
      d  = day(SampleDate),
      Value = ResultValue,
      chla = Value,  # TODO: fix this variable name
      epchc_station = paste(Source, Site, sep="."),
      Latitude = OriginalLatitude,
      Longitude = OriginalLongitude
    ) |>
    # Remove rows with missing critical data for mapping and analysis
    tidyr::drop_na(
      Latitude, Longitude,
      chla,
    ) |>
    # Map parameter names to standardized dashboard-compatible names
    # NOTE: These must match the names used in the filters in wq-dash.Rmd
    mutate(
      Parameter = case_when(
        # Nitrogen compounds
        Parameter == "Ammonia (N)"               ~ "Ammonia (N)",
        Parameter == "Ammonium (N)"              ~ "Ammonium",
        Parameter == "Nitrate-Nitrite (N)"       ~ "Nitrate+Nitrite",
        Parameter == "Nitrite (N)"               ~ "Nitrite",
        Parameter == "Nitrate (N)"               ~ "Nitrate",
        Parameter == "Nitrogen- Total"           ~ "Total Nitrogen",
        Parameter == "Nitrogen- Total Kjeldahl"  ~ "Total Kjeldahl Nitrogen",
        # Phosphorus compounds
        Parameter == "Orthophosphate (P)"        ~ "Orthophosphate",
        Parameter == "Phosphorus- Total"         ~ "Total Phosphorus",
        # Other water quality parameters
        Parameter == "Chlorophyll a"             ~ "Chlorophyll a",
        Parameter == "Turbidity"                 ~ "Turbidity",
        Parameter == "Silica"                    ~ "Silica",
        # Error handling for unmapped parameters
        TRUE ~ "PARAM MAPPING ERR"
      ))
      
  return(epcdata)
}