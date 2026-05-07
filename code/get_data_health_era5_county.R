#---
# get_data_health_era5_county.R
#
# This Rscript:
# * generate EDvisit counts per county (from counts per ZCTA)
# * combine EDvisit counts (per county) and ERA5 data (hourly, wider form)
# * add temp_min, temp_max, DTR, day-of-week, holiday
# * save the cleaned data for each county in a list (.rds)
# Note: 
# - For each county: fill missing dates, code associated EDvisit counts with 0
#
# Dependencies...
# data/raw/era5_cleaned_ric_county.rds
# data/raw/edvisit_county_richmond_msa.rds
# data/raw/us_holiday_1900_2100.csv
#
# Produces...
# data/process/data_health_era5_county.rds
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        chva.extras,    # supplementary functions
        magrittr
)

# data #------------------------------
ls_era5 <- readRDS(here("data/raw/era5_cleaned_ric_county.rds")) %>% 
        # named list -> nested tibble
        enframe() %>% 
        arrange(name) %>% 
        # nested tibble -> named list
        deframe()

ls_EDvisit <- readRDS(here("data/raw/edvisit_county_richmond_msa.rds")) %>%
        map(\(data) data %>% 
                    select(date = Incurred_Date,
                           EDvisit = EDvisit_total)) %>% 
        enframe() %>% 
        mutate(name = case_when(name == "Charles City" ~ "Charles City County",
                                name == "King And Queen County" ~ "King and Queen County",
                                TRUE ~ name)) %>% 
        deframe()

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

ls_era5_wider <- ls_era5 %>% 
        # loop for each county
        map(\(data) {
                ls_var_wider <- var_names %>% 
                        set_names(var_names) %>% 
                        # loop for each variable
                        map(\(variable) {
                                data %>% 
                                        select(date, time, all_of(variable)) %>% 
                                        pivot_wider(names_from = time,
                                                    names_prefix = paste0(variable, "_t"),
                                                    values_from = variable)
                        })
                # sequentially left join all data frames in the list by "date" column
                res <- ls_var_wider %>% reduce(left_join, by = "date")
                return(res)
        })

# era5 - temp_min, temp_max, temp_DTR #-----------------------
ls_DTR <- ls_era5 %>% 
        map(\(data) data %>% 
                    select(date, air_tempC) %>% 
                    nest(.by = date) %>% 
                    mutate(temp_minC = map_dbl(data,
                                               ~ .x %>% pull() %>% min(na.rm = TRUE)),
                           temp_maxC = map_dbl(data,
                                               ~ .x %>% pull() %>% max(na.rm = TRUE)),
                           DTR = temp_maxC - temp_minC) %>% 
                    select(-data)
            )

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

(df_holiday_flag <- ls_era5_wider[[1]] %>% 
                distinct(date) %>% 
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
(county_names <- names(ls_era5_wider))

ls_final <- county_names %>% 
        set_names(county_names) %>% 
        map(\(county) df_holiday_flag %>% 
                    left_join(ls_EDvisit[[county]],
                              by = c("date" = "date")) %>% 
                    left_join(ls_DTR[[county]], by = join_by(date)) %>%
                    left_join(ls_era5_wider[[county]], by = join_by(date))
            )

# save as .rds
saveRDS(ls_final,
        here("data/process/data_health_era5_county.rds"))

