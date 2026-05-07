#---
# get_data_station_raw.R
#
# This Rscript:
# * download weather observations from ASOS
# * save data as .csv
# Note: 
# - https://mesonet.agron.iastate.edu/request/download.phtml?network=IN__ASOS
# - [ASOS User's Guide](https://www.weather.gov/media/asos/aum-toc.pdf)
#
# Produces...
# data/raw/data_station_raw.csv
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        riem,           # get weather data from ASOS stations (airports)
        chva.extras     # supplementary functions
)

# get weather obs #----------------------------
ric_measures <- riem_measures(station = "RIC",
                              date_start = "2015-12-01",
                              date_end = "2023-01-01") %>%
        select(timestamp_utc = valid,
               air_tempF = tmpf,
               dew_point_tempF = dwpf,
               relative_humidity_pct = relh,
               wind_speed_knots = sknt,
               sea_level_pressure_millibar = mslp,
               apparent_tempF = feel
        )

# save as .csv #----------------------------
rio::export(ric_measures,
            here("data/raw/data_station_raw.csv"))

