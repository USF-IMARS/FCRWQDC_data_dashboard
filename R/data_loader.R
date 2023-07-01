load_station_data <- function(fpath){
  #load('data/epcdata.RData')
  station_data <- readr::read_csv(
    fpath
  ) |>
    dplyr::mutate(
      .keep = "none",
      Site = Site,
      bay_segment = Source,
      epchc_station = dplyr::row_number(), # Site,
      Latitude = Lat,
      Longitude = Long,
      start_year = `Sample Start Year`,
      end_year = `Sample End Year`,
      years_of_sampling = as.integer(end_year) - as.integer(start_year),
      current_sampling_status = `Currently Sampling?`,
      # = 'Notes',
    ) 
  return(station_data)
}

load_wq_data <- function (fpath)  {
  station_data <- load_station_data(
    'data/station_sampling_periods_for_all_programs.csv'
  )
  # samples are monthly but must be given a day, 
  # so we add the ASSUMED_DAY_OF_MONTH.
  ASSUMED_DAY_OF_MONTH <- 15
  epcdata <- readr::read_csv(
    fpath
  ) |>
    dplyr::mutate(
      .keep = "none",
      Site = Site,
      # bay_segment = Source,
      yr = Year,
      mo = Month,
      chla = Value,
      
      # = Parameter
      # = Units
      SampleTimeString = paste(yr, mo, ASSUMED_DAY_OF_MONTH, sep="-"),
      SampleTime = as.POSIXct(SampleTimeString, format="%Y-%m-%d")
      
      ,
    ) |>
    dplyr::left_join(
      # x = .,
      y = station_data,
    ) |>
    tidyr::drop_na(
      Latitude, Longitude,
      chla,
    )
  return(epcdata)
  # load('data/epcdata.RData')
}