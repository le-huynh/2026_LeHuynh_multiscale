#---
# model_mixmeta_era5_zcta_mean5.R
#
# This Rscript:
# * fit meta-regression model from zcta-specific models
# * save fitted model as .rds
# Note: use zcta-specific models of ZCTAs with EDvisit mean >= 5
#
# Dependencies...
# data/RDSmodel/gam_health_era5_zcta.rds
# data/process/data_health_era5_zcta.rds
# code/fn_crosspred.R
#
# Produces...
# data/RDSmodel/gam_health_era5_zcta_mean5.rds
# data/RDSmodel/mixmeta_era5_zcta_mean5.rds
# data/RDSmodel/pgam_meta_era5_zcta_mean5.rds
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
mod_zcta_all <- readRDS(here("data/RDSmodel/gam_health_era5_zcta.rds"))

ls_data <- readRDS(here("data/process/data_health_era5_zcta.rds"))

(zcta_mean5 <- ls_data %>%
        enframe(name = "zcta") %>%
        mutate(EDvisit_mean = map_dbl(value,
                                      \(data) data %>% 
                                          filter(!year %in% c(2015, 2023)) %>% 
                                          pull(EDvisit) %>% 
                                          mean(na.rm = TRUE))) %>% 
        filter(EDvisit_mean >= 5) %>% 
        pull(zcta))

mod_zcta_mean5 <- mod_zcta_all %>% keep_at(at = zcta_mean5)

saveRDS(mod_zcta_mean5, here("data/RDSmodel/gam_health_era5_zcta_mean5.rds"))

# ZCTA-specific coefficient #-------------
ls_coef_mean5 <- mod_zcta_mean5 %>% 
        map(\(data) data %>% 
                    coef() %>% 
                    as_tibble(rownames = "term") %>% 
                    filter(str_detect(term, "temp")) %>% 
                    select(value) %>%
                    rowid_to_column() %>% 
                    pivot_wider(names_prefix = "coef", 
                                names_from = rowid, 
                                values_from = value))

(coef_mean5 <- ls_coef_mean5 %>% 
                list_rbind(names_to = "rowname") %>% 
                column_to_rownames("rowname") %>% 
                as.matrix())

# ZCTA-specific varian-covariance matrix #--------------
ls_vcov_mean5 <- mod_zcta_mean5 %>% 
        map(\(data) data %>% 
                    vcov.gam() %>% 
                    as_tibble(rownames = "term") %>% 
                    filter(str_detect(term, "temp")) %>% 
                    select(contains("temp")) %>% 
                    as.matrix() %>% 
                    vechMat() %>% 
                    set_names(paste0("vcov", 1:6))
        )

(vcov_mean5 <- do.call(rbind, ls_vcov_mean5))

# fit meta-regression model #-----------
model0_mean5 <- mixmeta(coef_mean5 ~ 1, vcov_mean5, method = "ml")

# summary(model0_mean5)

saveRDS(model0_mean5, here("data/RDSmodel/mixmeta_era5_zcta_mean5.rds"))

# interpretation #-----------------------------------
# zcta-specific original temperature
ls_temp_mean5 <- map(mod_zcta_mean5,
                     \(data) data$model %>% pull(temp_maxC))

# DEFINE SPLINE TRANSFORMATION ORIGINALLY USED IN FIRST-STAGE MODELS
bvar_mean5 <- mod_zcta_mean5[["23231"]]$smooth[[2]] # temp_maxC

pgam_mean5 <- fn_crosspred(basis = bvar_mean5,
                           coef = coef(model0_mean5),
                           vcov = vcov(model0_mean5),
                           model.link="log",
                           cen = 21,
                           at = seq(from = -5, to = 40, length.out = 500))

saveRDS(pgam_mean5, here("data/RDSmodel/pgam_meta_era5_zcta_mean5.rds"))


