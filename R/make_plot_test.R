library(testthat)
source('R/make_plot.R')
test_that("make_plot can run", {
  source('R/data_loader.R')
  result <- make_plot(
    dataframe = load_wq_data('data/Merged_chla.csv'),
    varname = 'Chlorophyll-a',  # str: column name of variable to plot
    ylabel = 'Concentration (ug/L)',  # str: label for y axis
    station_colname = 'epchc_station',  # str: name of column with station ids
    selsit = '218'
  )
  print(result)
})

