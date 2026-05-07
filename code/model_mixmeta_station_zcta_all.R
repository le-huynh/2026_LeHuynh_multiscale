#---
# model_mixmeta_station_zcta_all.R
#
# This Rscript:
# * fit meta-regression model from zcta-specific models
# * save fitted model as .rds
#
# Dependencies...
# data/RDSmodel/gam_health_station_zcta.rds
# code/fn_crosspred.R
#
# Produces...
# data/RDSmodel/mixmeta_station_zcta_all.rds
# data/RDSmodel/pgam_meta_station_zcta_all.rds
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
mod_zcta_all <- readRDS(here("data/RDSmodel/gam_health_station_zcta.rds"))

# ZCTA-specific coefficient #-------------
ls_coef_all <- mod_zcta_all %>% 
        map(\(data) data %>% 
                    coef() %>% 
                    as_tibble(rownames = "term") %>% 
                    filter(str_detect(term, "temp")) %>% 
                    select(value) %>%
                    rowid_to_column() %>% 
                    pivot_wider(names_prefix = "coef", 
                                names_from = rowid, 
                                values_from = value))

(coef_all <- ls_coef_all %>% 
                list_rbind(names_to = "rowname") %>% 
                column_to_rownames("rowname") %>% 
                as.matrix())

# ZCTA-specific varian-covariance matrix #--------------
ls_vcov_all <- mod_zcta_all %>% 
        map(\(data) data %>% 
                    vcov.gam() %>% 
                    as_tibble(rownames = "term") %>% 
                    filter(str_detect(term, "temp")) %>% 
                    select(contains("temp")) %>% 
                    as.matrix() %>% 
                    vechMat() %>% 
                    set_names(paste0("vcov", 1:6))
        )

(vcov_all <- do.call(rbind, ls_vcov_all))

# fit meta-regression model #-----------
model0_all <- mixmeta(coef_all ~ 1, vcov_all, method = "ml")

# summary(model0_all)

saveRDS(model0_all, here("data/RDSmodel/mixmeta_station_zcta_all.rds"))

# interpretation #-----------------------------------
# zcta-specific original temperature
ls_temp_all <- map(mod_zcta_all,
                     \(data) data$model %>% pull(temp_maxC))

# DEFINE SPLINE TRANSFORMATION ORIGINALLY USED IN FIRST-STAGE MODELS
bvar_all <- mod_zcta_all[["23231"]]$smooth[[2]] # temp_maxC

pgam_all <- fn_crosspred(basis = bvar_all,
                           coef = coef(model0_all),
                           vcov = vcov(model0_all),
                           model.link="log",
                           cen = 21,
                           at = seq(from = -5, to = 40, length.out = 500))

saveRDS(pgam_all, here("data/RDSmodel/pgam_meta_station_zcta_all.rds"))


