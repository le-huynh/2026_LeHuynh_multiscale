#---
# get_data_station_missing.R
#
# This Rscript:
# * get maximum obs closest to 0000 → 2300 within 180-min time window
# * get daily maximum and minimum temperature → calculate diurnal temperature range
# * save data as .csv
#
# Dependencies...
# data/raw/data_station_raw.csv
#
# Produces...
# data/process/data_station_missing.csv
# data/process/data_station_interpolated.csv
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        weathermetrics, # convert between weather metrics
        chva.extras,    # supplementary functions
        tictoc          # track processing time
)

# data #----------------------------
(raw_data <- rio::import(here("data/raw/data_station_raw.csv")) %>% tibble())

(wdata <- raw_data %>% 
  mutate(# replace erroneous air_temperature on 2018-04-25 with NA
         air_tempF = case_when(timestamp_utc == ymd_hms("2018-04-25 13:21:00") ~ NA_integer_,
                               TRUE ~ air_tempF),
         # convert temperature Fahrenheit to Celsius
         across(.cols = contains("temp"),
                .fns = fahrenheit.to.celsius)) %>%
  # rename temperature columns: Fahrenheit to Celsius
  rename_with(.cols = contains("temp"),
              ~ str_replace(.x, "F", "C")))

# get closest obs at specific time #-----------------
(working_data <- wdata %>% 
                mutate(date = date(timestamp_utc)) %>% 
                nest(.by = date))

target_time <- 0:23
(vars <- names(wdata)[-1])
obs_list <- vector("list", length(vars))

tic()
for (i in seq_along(vars)) {
  obs_list[[i]] <- map(
    target_time,
    function(target_time) {
      working_data %>%
        mutate(!!paste0(vars[[i]],
                        "_t",
                        target_time) := map_dbl(data,
                                                function(data) {
                                                  get_weather_obs(
                                                    data = data,
                                                    target_column = !!rlang::sym(vars[[i]]),
                                                    timestamp_column = timestamp_utc,
                                                    target_hour = target_time,
                                                    target_minute = 0,
                                                    time_window = 90)
                                                  }
                                                ))
    }) %>%
    reduce(full_join, by = c("date", "data"))
}
toc()

(df_obs <- reduce(obs_list,
                  full_join,
                  by = c("date", "data")) %>% 
                select(-data))

# get DTR #-----------------
(df_DTR <- wdata %>% 
  select(timestamp_utc, air_tempC) %>% 
  mutate(date = date(timestamp_utc)) %>% 
  nest(.by = date) %>% 
  mutate(temp_minC = map_dbl(data, ~min(.x$air_tempC, na.rm = TRUE)),
         temp_maxC = map_dbl(data, ~max(.x$air_tempC, na.rm = TRUE)),
         DTR = temp_maxC - temp_minC) %>% 
  select(date, temp_minC, temp_maxC, DTR))

# combine obs-minmax-DTR #-------------
(df_obs_DTR <- df_obs %>%
  full_join(df_DTR,
            by = "date") %>%
  select(date,
         temp_minC, temp_maxC, DTR, contains("temp"),
         everything()))

# save as .csv
rio::export(df_obs_DTR,
            here("data/process/data_station_missing.csv"))


