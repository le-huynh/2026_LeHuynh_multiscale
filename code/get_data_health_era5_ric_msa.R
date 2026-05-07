#---
# get_data_health_era5_ric_msa.R
#
# This Rscript:
# * combine EDvisit counts (full Richmond MSA) and ERA5 data (hourly, wider form)
# * add temp_min, temp_max, DTR, day-of-week, holiday
# * save as .csv
#
# Dependencies...
# data/raw/edvisit_full_richmond_msa.csv
# data/raw/era5_cleaned_ric_msa.csv
# data/raw/us_holiday_1900_2100.csv
#
# Produces...
# data/process/data_health_era5_ric_msa.csv
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

(df_era5 <- read_csv(here("data/raw/era5_cleaned_ric_msa.csv")) %>% 
                tibble())

(us_holiday <- rio::import(here("data/raw/us_holiday_1900_2100.csv")) %>% 
                tibble() %>%
                mutate(date = ymd(date)) %>% 
                select(holiday, year, date))

# era5 - wider form #-----------------------
var_names <- c("air_tempC",
               "dew_point_tempC",
               "apparent_tempC",
               "relative_humidity_pct",
               "wind_speed_knots",
               "sea_level_pressure_millibar"
)

(df_era5_wider <- var_names %>% 
        set_names(var_names) %>% 
        # loop for each variable
        map(\(variable) {
                df_era5 %>% 
                        select(date, time, all_of(variable)) %>% 
                        pivot_wider(names_from = time,
                                    names_prefix = paste0(variable, "_t"),
                                    values_from = variable)
        }) %>% 
        # sequentially left join all data frames in the list by "date" column
        reduce(left_join, by = "date"))

# era5 - temp_min, temp_max, temp_DTR #-----------------------
(df_DTR <- df_era5 %>%
        select(date, air_tempC) %>% 
        nest(.by = date) %>% 
        mutate(temp_minC = map_dbl(data,
                                   ~ .x %>% pull() %>% min(na.rm = TRUE)),
               temp_maxC = map_dbl(data,
                                   ~ .x %>% pull() %>% max(na.rm = TRUE)),
               DTR = temp_maxC - temp_minC) %>% 
        select(-data))

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

(df_holiday_flag <- df_era5_wider %>% 
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
        left_join(df_DTR, by = join_by(date)) %>%
        left_join(df_era5_wider, by = join_by(date)))

# save as .csv
rio::export(df_final,
            here("data/process/data_health_era5_ric_msa.csv"))

