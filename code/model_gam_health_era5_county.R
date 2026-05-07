#---
# model_gam_health_era5_county.R
#
# This Rscript:
# * fit GAM for each county/independent city in Richmond MSA using ERA5 data
# * save fitted model as .rds
#
# Dependencies...
# data/process/data_health_era5_county.rds
#
# Produces...
# data/RDSmodel/gam_health_era5_county.rds
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        mgcv
)

# data #-----------
data <- readRDS(here("data/process/data_health_era5_county.rds"))

wdf <- data %>% 
        map(\(data) data %>% 
                    filter(!year %in% c(2015, 2023)) %>% 
                    rowid_to_column(var = "trend"))

# model fitting #--------------
ls_model <- wdf %>% 
        map(\(data){
        
        model <- gam(EDvisit ~ s(trend, k = 7 * 4) +
                             s(temp_maxC, k = 4) +
                             s(DTR, k = 4) +
                             as.factor(day_of_week) +
                             as.factor(holiday_flag),
                     family = "quasipoisson",
                     method = "REML",
                     data = data)
        
        return(model)
})

# save fitted model
saveRDS(ls_model, here("data/RDSmodel/gam_health_era5_county.rds"))

