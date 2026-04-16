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
    tidyr::drop_na(
      Latitude, Longitude,
      chla,
    ) |>
    # map from new names to old names
    # NOTE: these must match the names used in the filters in wq-dash.Rmd
    mutate(
      Parameter = case_when(
        Parameter == "Ammonia (N)"               ~ "Ammonia (N)",
        Parameter == "Ammonium (N)"              ~ "Ammonium",
        Parameter == "Chlorophyll a"             ~ "Chlorophyll a",
        Parameter == "Turbidity"                 ~ "Turbidity",
        Parameter == "Silica"                    ~ "Silica",
        Parameter == "Nitrate-Nitrite (N)"       ~ "Nitrate+Nitrite",
        Parameter == "Nitrite (N)"               ~ "Nitrite",
        Parameter == "Nitrate (N)"               ~ "Nitrate",
        Parameter == "Nitrogen- Total"           ~ "Total Nitrogen",
        Parameter == "Nitrogen- Total Kjeldahl"  ~ "Total Kjeldahl Nitrogen",
        Parameter == "Orthophosphate (P)"        ~ "Orthophosphate",
        Parameter == "Phosphorus- Total"         ~ "Total Phosphorus",
        TRUE ~ "PARAM MAPPING ERR"
      ))
  return(epcdata)
}