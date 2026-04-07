load_wq_cleaned_df <- function(fpath){
  # load data from cleaned df csv file.
  epcdata <- readr::read_csv(
    fpath
  ) |>
    dplyr::mutate(
      Site = Site,
      bay_segment = Source,
      Source = Source,
      Parameter = Parameter,
      Units = Units,
      Sample.Depth = `Sample.Depth`,
      SampleTimeString = as.character(datetime),
      SampleTime = datetime,
      yr = year(datetime),
      mo = month(datetime),
      d  = day(datetime),
      Value = Value,
      chla = Value,  # TODO: fix this variable name
      epchc_station = paste(Source, Site, sep="."),
      Latitude = Latitude,
      Longitude = Longitude
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