#---
# model_mixmeta_station_county.R
#
# This Rscript:
# * fit meta-regression model from county-specific models
# * save fitted model as .rds
#
# Dependencies...
# data/RDSmodel/gam_health_station_county.rds
# code/fn_crosspred.R
#
# Produces...
# data/RDSmodel/mixmeta_station_county.rds
# data/RDSmodel/pgam_meta_station_county.rds
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        mixmeta,
        mgcv,
        dlnm
)

source(here("code/fn_crosspred.R"))

# data #----------------------------
mod_county <- readRDS(here("data/RDSmodel/gam_health_station_county.rds"))

# county-specific coefficient #-------------
ls_coef <- mod_county %>% 
        map(\(data) data %>% 
                    coef() %>% 
                    as_tibble(rownames = "term") %>% 
                    filter(str_detect(term, "temp")) %>% 
                    select(value) %>%
                    rowid_to_column() %>% 
                    pivot_wider(names_prefix = "coef", 
                                names_from = rowid, 
                                values_from = value))

(coef <- ls_coef %>% 
                list_rbind(names_to = "rowname") %>% 
                column_to_rownames("rowname") %>% 
                as.matrix())

# county-specific varian-covariance matrix #--------------
ls_vcov <- mod_county %>% 
        map(\(data) data %>% 
                    vcov.gam() %>% 
                    as_tibble(rownames = "term") %>% 
                    filter(str_detect(term, "temp")) %>% 
                    select(contains("temp")) %>% 
                    as.matrix() %>% 
                    vechMat() %>% 
                    set_names(paste0("vcov", 1:6))
        )

(vcov <- do.call(rbind, ls_vcov))

# fit meta-regression model #-----------
model0 <- mixmeta(coef ~ 1, vcov, method = "ml")

# summary(model0)

saveRDS(model0, here("data/RDSmodel/mixmeta_station_county.rds"))

# interpretation #-----------------------------------
# county-specific original temperature
ls_temp <- map(mod_county,
                     \(data) data$model %>% pull(temp_maxC))

# DEFINE SPLINE TRANSFORMATION ORIGINALLY USED IN FIRST-STAGE MODELS
bvar <- mod_county[["Henrico County"]]$smooth[[2]] # temp_maxC

pgam <- fn_crosspred(basis = bvar, 
                     coef = coef(model0),
                     vcov = vcov(model0),
                     model.link="log",
                     cen = 21,
                     at = seq(from = -5, to = 40, length.out = 500))

saveRDS(pgam, here("data/RDSmodel/pgam_meta_station_county.rds"))


