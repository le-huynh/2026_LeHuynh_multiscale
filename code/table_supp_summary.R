#---
# table_supp_summary.R
#
# This Rscript: generate summary table for Supplementary Materials
#
# Dependencies...
# data/process/data_health_station.csv
# data/process/data_health_era5_ric_msa.csv
# data/process/data_health_era5_cluster.rds
# data/process/data_health_era5_county.rds
# data/process/data_health_era5_zcta.rds
#
# Produces...
# data/for_manuscript/supp_summary.csv
# data/for_manuscript/df_temperature_diff.csv
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        skimr
)

# data #----------------------
(df_station <- rio::import(here("data/process/data_health_station.csv")) %>% 
        as_tibble() %>% 
        filter(!year %in% c(2015, 2023)) %>% 
        mutate(date = date(date)))

(df_era5_msa <- rio::import(here("data/process/data_health_era5_ric_msa.csv")) %>% 
        as_tibble() %>%
        filter(!year %in% c(2015, 2023)) %>% 
        mutate(date = date(date)))

ls_era5_cluster <- readRDS(here("data/process/data_health_era5_cluster.rds")) %>% 
        map(\(data) data %>% filter(!year %in% c(2015, 2023)))

ls_era5_county <- readRDS(here("data/process/data_health_era5_county.rds")) %>% 
        map(\(data) data %>% filter(!year %in% c(2015, 2023)))

ls_era5_zcta <- readRDS(here("data/process/data_health_era5_zcta.rds")) %>% 
        map(\(data) data %>% filter(!year %in% c(2015, 2023)))

wls <- c(list(full_msa_era5 = df_era5_msa),
         ls_era5_cluster,
         ls_era5_county,
         ls_era5_zcta)

# table - summary #------------------------
## EDvisit #---------------------
(df_edvisit_summary <- wls %>% 
        enframe(name = "location") %>% 
        mutate(EDvisit = map(value,
                             \(data) data %>% 
                                     summarise(total = sum(EDvisit, na.rm = TRUE),
                                               mean = mean(EDvisit, na.rm = TRUE),
                                               sd = sd(EDvisit, na.rm = TRUE),
                                               min = min(EDvisit, na.rm = TRUE),
                                               max = max(EDvisit, na.rm = TRUE)))) %>% 
        unnest(EDvisit) %>% 
        select(-value))

## era5 data #-----------------------
(df_era5_summary <- wls %>% 
        enframe(name = "location") %>% 
        mutate(temp_maxC = map(value,
                               \(data) data %>% 
                                       skimr::skim_without_charts(temp_maxC) %>% 
                                       select(contains("numeric")) %>%
                                       as.data.frame() %>% 
                                       rename_with(~ str_remove(.x, "numeric.")))) %>% 
        unnest(temp_maxC) %>% 
        select(-value))

## station data #-----------------------
(df_station_summary <- df_station %>% 
        skimr::skim_without_charts(temp_maxC) %>% 
        select(contains("numeric")) %>% 
        as.data.frame() %>% 
        rename_with(~ str_remove(.x, "numeric.")) %>% 
        rename_with(~ paste0("station_", .x)))

## combine #------------------
(df_combined <- df_edvisit_summary %>% 
        rename_with(.cols = -location,
                    ~paste0("edvisit_", .x)) %>% 
        full_join(df_era5_summary %>% 
                          rename_with(.cols = -location,
                                      ~paste0("era5_", .x)),
                  by = join_by(location)) %>% 
        bind_cols(df_station_summary))

## save .csv
rio::export(df_combined,
            here("data/for_manuscript/supp_summary.csv"))

# table - temp diff #--------------------------
(df_temp_diff <- wls %>% 
        enframe(name = "location") %>%
        mutate(diff = map(value,
                          \(data) data %>% 
                                  select(date, era5_temp_maxC = temp_maxC) %>% 
                                  left_join(df_station %>% 
                                                    select(date, station_temp_maxC = temp_maxC),
                                            by = join_by(date)) %>% 
                                  mutate(diff = abs(station_temp_maxC - era5_temp_maxC))
        )) %>% 
        select(-value) %>% 
        unnest(diff))

## save .csv
rio::export(df_temp_diff,
            here("data/for_manuscript/df_temperature_diff.csv"))

