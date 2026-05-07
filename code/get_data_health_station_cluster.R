#---
# get_data_health_station_cluster.R
#
# This Rscript:
# * combine EDvisit counts (per cluster) and station data (hourly, wider form)
# * add temp_min, temp_max, DTR, day-of-week, holiday
# * save the cleaned data for each cluster in a list (.rds)
# Note: 
# - For each cluster: fill missing dates, code associated EDvisit counts with 0
#
# Dependencies...
# data/process/data_health_era5_cluster.rds
# data/process/data_health_station.csv
#
# Produces...
# data/process/data_health_station_cluster.rds
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse      # data management and visualization
)

# data #------------------------------
(df_health_station <- rio::import(here("data/process/data_health_station.csv")) %>% 
         tibble())

ls_health_era5 <- readRDS(here("data/process/data_health_era5_cluster.rds"))

# data cleaning #-------------------
# station data
(df_station <- df_health_station %>%
        mutate(date = lubridate::date(date)) %>% 
        select(-EDvisit))

# combine station data and EDvisit for each cluster
ls_final <- ls_health_era5 %>%
        map(\(data) data %>% 
                    filter(year != 2023) %>% 
                    select(date, EDvisit) %>%
                    left_join(df_station,
                              by = join_by(date)) %>%
                    relocate(EDvisit, .after = holiday_flag))

# save as .rds
saveRDS(ls_final,
        here("data/process/data_health_station_cluster.rds"))

