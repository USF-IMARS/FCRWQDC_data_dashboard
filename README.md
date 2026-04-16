# README
Prototype dashboard for viewing water quality parameters around FL.

Data shown is ingested from multiple providers through processes documented in https://github.com/USF-IMARS/FCRWQDC-SEACAR-analysis.

Older versions of this file were prepared with https://github.com/USF-IMARS/FCRWQDC_data_ingest.


For this application to work the files produced by the upstream data processing must be placed in the `./data/` directory.
These files are not tracked using git due to the large size of the files.

```bash
cp ../FCRWQDC-SEACAR-analysis/data/exports/dashboardStations.csv ./data/.
cp -R ../FCRWQDC-SEACAR-analysis/data/exports/stationData ./data/.
```

The files must be referenced in R code in order for the RStudio shinyapps pub interface to pick them up.
To accomplish this run the following after putting the stationData files in place:

```bash
printf '# Station data files - referenced here so shinyapps.io bundles them\nstationFiles <- c(\n' > listOfStationFiles.R
ls data/stationData/ | awk 'NR>1{print prev","} {prev="  \"data/stationData/"$0"\""} END{print prev}' >> listOfStationFiles.R
printf ')\n' >> listOfStationFiles.R
```


# Testing
```r
source("./run_tests.R")
```


# Attributions
This dashboard was based on [TBEP's wq-dashboard](https://github.com/tbep-tech/wq-dash).
Info below is held over from that repo.

[![build](https://github.com/tbep-tech/wq-dash/actions/workflows/databuild.yaml/badge.svg)](https://github.com/tbep-tech/wq-dash/actions/workflows/databuild.yaml)
[![DOI](https://zenodo.org/badge/223773148.svg)](https://zenodo.org/badge/latestdoi/223773148)

Materials for TBEP water quality dashboard.  

[Shiny](http://shiny.tbep.org/wq-dash/)


---------------------------------------------------

Data shown here has undergone multiple iterations of ingestion, analysis, and cleaning.
Some data cleaning is documented in [USF-IMaRS/WIN_data_ingest](https://github.com/USF-IMARS/WIN_data_ingest).

Older analyses include:

* [older data compilation efforts using data from previous years](https://github.com/USF-IMARS/dep-wq-data-report)
* [comparison of older data to data from SEACAR](https://github.com/USF-IMARS/dep-seacar-data-compare)
