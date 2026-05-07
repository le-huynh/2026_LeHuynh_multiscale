#---
# get_data_station_interpolated.R
#
# This Rscript:
# * linear interpolate single missing obs, retain two or more consecutive missing obs
# * save data as .csv
#
# Dependencies...
# data/process/data_station_missing.csv
#
# Produces...
# data/process/data_station_interpolated.csv
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        chva.extras     # supplementary functions
)

# data #----------------------------
(data_missing <- rio::import(here("data/process/data_station_missing.csv")) %>% 
         tibble())

# get dates with NAs #---------------
var_names <- c("air_tempC",
               "dew_point_tempC",
               "apparent_tempC",
               "relative_humidity",
               "wind_speed",
               "sea_level_pressure")

list_missing <- map(.x = var_names,
                    .f = ~{data_missing %>% 
                                    select(date, contains(.x)) %>% 
                                    filter(if_any(-date, ~is.na(.)))}) %>% 
        purrr::set_names(var_names)

# interpolate NAs #-------------
list_interpolated <- map2(.x = list_missing,
                          .y = var_names,
                          .f = ~ interpolate_na(data = .x,
                                                var1 = "date",
                                                vars2 = contains(.y)))

# update data with interpolated values #----------
df_final <- data_missing %>% 
        rows_update(list_interpolated[["air_tempC"]], by = "date") %>%
        rows_update(list_interpolated[["dew_point_tempC"]], by = "date") %>%
        rows_update(list_interpolated[["apparent_tempC"]], by = "date") %>% 
        rows_update(list_interpolated[["relative_humidity"]], by = "date") %>% 
        rows_update(list_interpolated[["wind_speed"]], by = "date") %>% 
        rows_update(list_interpolated[["sea_level_pressure"]], by = "date") 

# save data as .csv #---------
rio::export(df_final, "data/process/data_station_interpolated.csv")

