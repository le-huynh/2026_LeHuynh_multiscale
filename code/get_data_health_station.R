#---
# get_data_health_station.R
#
# This Rscript:
# * combine EDvisit counts and weather station data
# * add day-of-week and holiday
# * save as .csv
#
# Dependencies...
# data/raw/edvisit_full_richmond_msa.csv
# data/process/data_station_interpolated.csv
# data/raw/us_holiday_1900_2100.csv
#
# Produces...
# data/process/data_health_station.csv
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        chva.extras     # supplementary functions
)

# data #------------------------------
(df_EDvisit <- read_csv(here("data/raw/edvisit_full_richmond_msa.csv")) %>%
                    select(date = Incurred_Date,
                           EDvisit = EDvisit_total))

(df_station <- rio::import(here("data/process/data_station_interpolated.csv")) %>% 
                tibble() %>% 
                mutate(date = ymd(date)))

(us_holiday <- rio::import(here("data/raw/us_holiday_1900_2100.csv")) %>% 
                tibble() %>%
                mutate(date = ymd(date)) %>% 
                select(holiday, year, date))

# holiday #---------------
(df_holiday <- us_holiday %>% 
        filter(year %in% 2015:2022,
               holiday %in% c("New Year",
                              "Memorial Day",
                              "Independence Day",
                              "Labor Day",
                              "Thanksgiving Day",
                              "Christmas Day")) %>% 
         select(date, holiday))

(df_holiday_flag <- df_station %>% 
        select(date) %>% 
        mutate(year = lubridate::year(date),
               month = lubridate::month(date),
               day = lubridate::day(date),
               day_of_week = lubridate::wday(date)) %>%
        left_join(df_holiday, by = join_by(date)) %>%
        mutate(holiday_flag = if_else(
                       !is.na(holiday) | 
                               lead(!is.na(holiday), default = FALSE) | 
                               lag(!is.na(holiday), default = FALSE), 
                        1, 
                        0)))

# combine #-------------------
(df_final <- df_holiday_flag %>% 
        left_join(df_EDvisit, by = c("date" = "date")) %>%
        left_join(df_station, by = "date"))

# save as .csv
rio::export(df_final,
            here("data/process/data_health_station.csv"))

