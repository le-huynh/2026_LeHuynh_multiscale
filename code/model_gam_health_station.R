#---
# model_gam_health_station.R
#
# This Rscript:
# * fit GAM for full Richmond MSA using RIC station data
# * generate prediction for max_temperature using dlnm::crosspred()
# * save fitted model and prediction as .rds
#
# Dependencies...
# data/process/data_health_station.csv
#
# Produces...
# data/RDSmodel/gam_health_station.rds
# data/RDSmodel/pgam_health_station.rds
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        mgcv,
        dlnm
)

# data #-----------
(data <- rio::import(here("data/process/data_health_station.csv")) %>% 
         tibble())

(wdf <- data %>% 
                filter(!year %in% c(2015, 2023)) %>% 
                rowid_to_column(var = "trend"))

# model fitting #--------------
mod <- gam(EDvisit ~ s(trend, k = 7 * 4) + 
                        s(temp_maxC, k = 4) + 
                        s(DTR, k = 4) +
                        as.factor(day_of_week) +
                        as.factor(holiday_flag),
                family = "quasipoisson",
                method = "REML",
                data = wdf)

# save fitted model
saveRDS(mod, here("data/RDSmodel/gam_health_station.rds"))

# generate prediction for max_temperature using dlnm::crosspred()
pgam <- crosspred("temp_maxC",
                  model = mod,
                  # reference value for prediction
                  cen = 21,
                  # range of values for prediction
                  at = seq(from = -5, to = 40, length.out = 500))

# save prediction
saveRDS(pgam, here("data/RDSmodel/pgam_health_station.rds"))


