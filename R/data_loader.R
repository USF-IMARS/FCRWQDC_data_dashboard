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

load_wq_cleaned_df <- function(fpath){
  # load data from cleaned df csv file.
  # example file:
  #
  # (base) tylar@tylar-gram:~/xtra/repos/wq-dash$ head data/df_cleaned.csv 
  # "...1","Source","Site","Latitude","Longitude","Month","Year","Parameter","Value","Units","Total Depth","Sample Depth","Trend Analysis","Continuous","Start Date","End Date","verbatimValue","VerbatimLatitude","verbatimLongitude"
  # 1,"AOML","1",25.6433,-80.1267,1,1998,"Chlorophyll a",1.00488,"ug/L",NA,0,"Yes","Yes",1998,"Present","1.00488","25.643
  ASSUMED_DAY_OF_MONTH <- 15
  epcdata <- readr::read_csv(
    fpath
  ) |>
    dplyr::mutate(
      Site = Site,
      bay_segment = Source,
      yr = Year,
      mo = Month,
      d  = Day,
      chla = Value,  # TODO: fix this
      epchc_station = paste(Source, Site, sep="."),
      SampleTimeString = paste(yr, mo, d, sep="-"),
      SampleTime = as.POSIXct(SampleTimeString, format="%Y-%m-%d")
    ) |>
    tidyr::drop_na(
      Latitude, Longitude,
      chla,
    ) |>
    # drop specific data providers that we don't want to show
    filter(!Source %in% c("21FLWQA", "BBWW"))
  return(epcdata)
}

load_wq_data_from_merged_param_files <- function (fpath)  {
  # loads data from a file like `Merged_chla.csv`
  # expected columns:
  #
  # (base) tylar@tylar-gram:~/xtra/repos/wq-dash$ head data/Merged_chla.csv 
  # Source,Site,Month,Year,Parameter,Value,Units,Total Depth,Sample Depth
  # AOML,1,1,1998,chla,1.005,ug/L,,
  # AOML,2,1,1998,chla,0.556,ug/L,,
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