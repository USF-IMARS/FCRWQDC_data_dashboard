library(lubridate)
library(readr)
library(dplyr)
library(tidyr)

#' Load Station-Specific Water Quality Data
#'
#' This function loads water quality data for a specific station from the individual
#' station CSV files in the data/stationData directory. It processes the data to match
#' the same format as the main dataframe for consistency in plotting.
#'
#' @param program_name Character string specifying the program name (e.g., "AOML_SFPSSS")
#' @param location_id Character string specifying the location ID (e.g., "1")
#'
#' @return A data frame containing processed water quality data for the specific station
#'         with the same column structure as load_wq_cleaned_df()
#'
load_station_data <- function(program_name, location_id) {
  # handle the special case of `/` in the location_id
  location_id <- gsub("/", "_", location_id)
  
  # Construct the file path for the station-specific CSV
  station_file_path <- here::here("data/stationData", paste0(program_name, ".", location_id, ".csv"))
  
  # Check if the file exists
  if (!file.exists(station_file_path)) {
    warning(paste("Station data file not found:", station_file_path))
    return(NULL)
  }
  
  # Load the station-specific CSV data
  station_data <- readr::read_csv(station_file_path) %>%
    dplyr::mutate(
      Site = location_id,
      bay_segment = program_name,
      Source = program_name,
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
      chla = Value,  # TODO: fix this variable name - should be parameter-specific
      epchc_station = paste(Source, Site, sep="."),
      Latitude = NA,  # Station files don't contain coordinates
      Longitude = NA  # Station files don't contain coordinates
    ) %>%
    # Apply the same parameter mapping as the main data loader
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
        # Keep other parameters as-is (including Salinity, Water Temperature, pH, etc.)
        TRUE ~ Parameter
      ))
  
  # Return the processed station data
  return(station_data)
}

#' Load Dashboard Stations Data
#'
#' This function loads station metadata from the dashboardStations CSV file and processes
#' it for use in the dashboard map and filtering. The file contains counts of data points
#' for each parameter at each station.
#'
#' @param fpath Character string specifying the file path to the dashboardStations CSV file.
#'              Typically uses "data/dashboardStations.csv" for the dashboard.
#'
#' @return A data frame containing processed station metadata with parameter counts
#'
#' @export
load_dashboard_stations <- function(fpath) {
  # Load the dashboard stations data
  stations_data <- readr::read_csv(fpath) %>%
    dplyr::mutate(
      epchc_station = paste(ProgramName, ProgramLocationID, sep="."),
      Latitude = OriginalLatitude,
      Longitude = OriginalLongitude
    )
  
  return(stations_data)
}