#---
# plot_heat_cold.R
#
# This Rscript: generate plot to compare relative risk at 1st (cold) and 99th 
# (heat) temperature percentile
#
# Dependencies...
# code/fn_cal_relative_risk.R
# code/fn_crosspred.R
# data/RDSmodel/gam_health_station.rds
# data/RDSmodel/gam_health_station_cluster.rds
# data/RDSmodel/gam_health_station_county.rds
# data/RDSmodel/gam_health_station_zcta.rds
# data/RDSmodel/gam_health_station_zcta_mean5.rds
# data/RDSmodel/gam_health_era5.rds
# data/RDSmodel/gam_health_era5_cluster.rds
# data/RDSmodel/gam_health_era5_county.rds
# data/RDSmodel/gam_health_era5_zcta.rds
# data/RDSmodel/gam_health_era5_zcta_mean5.rds
# data/RDSmodel/mixmeta_station_county.rds
# data/RDSmodel/mixmeta_station_cluster.rds
# data/RDSmodel/mixmeta_station_zcta_all.rds
# data/RDSmodel/mixmeta_station_zcta_mean5.rds
# data/RDSmodel/mixmeta_era5_county.rds
# data/RDSmodel/mixmeta_era5_cluster.rds
# data/RDSmodel/mixmeta_era5_zcta_all.rds
# data/RDSmodel/mixmeta_era5_zcta_mean5.rds
#
# Produces...
# results/figures/fig_heat_cold.pdf
# data/for_manuscript/RR_heat_cold.RDS
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        lehuynh,
        mgcv,
        dlnm,
        patchwork
)

source(here("code/fn_cal_relative_risk.R"))
source(here("code/fn_crosspred.R"))

# function #----------------------
fn_df_meta <- function(data, data_type){
        
        res <- tibble(name = "wmeta",
                      temp_maxC = data$predvar,
                      rr = data$allRRfit,
                      rr_lower_ci = data$allRRlow,
                      rr_upper_ci = data$allRRhigh,
                      data_type = data_type,
                      model_type = "Pooled RR")
        
        return(res)
}

fn_cal_RR_meta <- function(mod_gam,
                           mod_meta,
                           cen = 21,
                           temp_range){
        
        # DEFINE SPLINE TRANSFORMATION ORIGINALLY USED IN FIRST-STAGE MODELS
        bvar <- mod_gam$smooth[[2]] # temp_maxC
        
        pgam <- fn_crosspred(basis = bvar, 
                             coef = dlnm::coef.crosspred(mod_meta),
                             vcov = dlnm::vcov.crosspred(mod_meta),
                             model.link="log",
                             cen = cen,
                             at = temp_range)
        
        return(pgam)
}

# data #-----------------------------
## location-specific GAM results #----------
# station data
mod_station_full <- readRDS(here("data/RDSmodel/gam_health_station.rds"))
mod_station_cluster <- readRDS(here("data/RDSmodel/gam_health_station_cluster.rds"))
mod_station_county <- readRDS(here("data/RDSmodel/gam_health_station_county.rds"))
mod_station_zcta_all <- readRDS(here("data/RDSmodel/gam_health_station_zcta.rds"))
mod_station_zcta_mean5 <- readRDS(here("data/RDSmodel/gam_health_station_zcta_mean5.rds"))

# ERA5 data
mod_era5_full <- readRDS(here("data/RDSmodel/gam_health_era5.rds"))
mod_era5_cluster <- readRDS(here("data/RDSmodel/gam_health_era5_cluster.rds"))
mod_era5_county <- readRDS(here("data/RDSmodel/gam_health_era5_county.rds"))
mod_era5_zcta_all <- readRDS(here("data/RDSmodel/gam_health_era5_zcta.rds"))
mod_era5_zcta_mean5 <- readRDS(here("data/RDSmodel/gam_health_era5_zcta_mean5.rds"))

## meta-regression models #------------
# station data
mod_meta_station_county <- readRDS(here("data/RDSmodel/mixmeta_station_county.rds"))
mod_meta_station_cluster <- readRDS(here("data/RDSmodel/mixmeta_station_cluster.rds"))
mod_meta_station_zcta_all <- readRDS(here("data/RDSmodel/mixmeta_station_zcta_all.rds"))
mod_meta_station_zcta_mean5 <- readRDS(here("data/RDSmodel/mixmeta_station_zcta_mean5.rds"))

# ERA5 data
mod_meta_era5_county <- readRDS(here("data/RDSmodel/mixmeta_era5_county.rds"))
mod_meta_era5_cluster <- readRDS(here("data/RDSmodel/mixmeta_era5_cluster.rds"))
mod_meta_era5_zcta_all <- readRDS(here("data/RDSmodel/mixmeta_era5_zcta_all.rds"))
mod_meta_era5_zcta_mean5 <- readRDS(here("data/RDSmodel/mixmeta_era5_zcta_mean5.rds"))

## 1st and 99th temperature percentiles #--------------
ref_temp <- 21

(temp_range <- mod_station_full$model %>%
        pull(temp_maxC) %>%
        quantile(probs = c(0.01, 0.99)))

## RR from location-specific models #-----------------------
# station data
(rr_station_full <- cal_relative_risk(model = mod_station_full,
                                     ref_temp = ref_temp,
                                     temp_range = temp_range))

# ERA5 data
(rr_era5_full <- cal_relative_risk(model = mod_era5_full,
                                  ref_temp = ref_temp,
                                  temp_range = temp_range))

## RR from meta-regression models #-----------------------
(wdf_meta <- pmap(list(list(mod_station_cluster[[1]],
                          mod_station_county[["Henrico County"]],
                          mod_station_zcta_all[["23231"]],
                          mod_station_zcta_mean5[["23231"]],
                          mod_era5_cluster[[1]],
                          mod_era5_county[["Henrico County"]],
                          mod_era5_zcta_all[["23231"]],
                          mod_era5_zcta_mean5[["23231"]]),
                     list(mod_meta_station_cluster,
                          mod_meta_station_county,
                          mod_meta_station_zcta_all,
                          mod_meta_station_zcta_mean5,
                          mod_meta_era5_cluster,
                          mod_meta_era5_county,
                          mod_meta_era5_zcta_all,
                          mod_meta_era5_zcta_mean5),
                     data_type = c("Station data - Clusters",
                                   "Station data - Counties",
                                   "Station data - All ZCTAs",
                                   "Station data - Selected ZCTAs",
                                   "ERA5 data - Clusters",
                                   "ERA5 data - Counties",
                                   "ERA5 data - All ZCTAs",
                                   "ERA5 data - Selected ZCTAs")),
                        \(mod_gam, mod_meta, data_type) {
                                
                                res <- fn_cal_RR_meta(mod_gam = mod_gam,
                                                      mod_meta = mod_meta,
                                                      cen = 21,
                                                      temp_range = temp_range) %>% 
                                        fn_df_meta(data_type = data_type)
                                
                                return(res)
                        }
                        ) %>% 
        list_rbind() %>% 
        select(-name, -model_type))

# plot - heat & cold #-------------------
## data #--------------------
(wdf_full_msa <- map2(.x = list(rr_era5_full,
                                rr_station_full),
                      .y = c("ERA5 data - Full Richmond MSA",
                             "Station data - Full Richmond MSA"),
                      \(data, data_type) data %>% 
                              select(temp_maxC,
                                     rr,
                                     rr_lower_ci,
                                     rr_upper_ci) %>% 
                              mutate(data_type = data_type)) %>% 
                list_rbind())

(wdf_plot_rr <- bind_rows(wdf_full_msa,
          wdf_meta) %>%
        mutate(group = case_when(temp_maxC == temp_range[1] ~ "Cold",
                                 temp_maxC == temp_range[2] ~ "Heat"),
               group = fct_relevel(group, c("Cold", "Heat")),
               label = data_type) %>% 
        separate(data_type,
                 into = c("data_type",
                          "spatial_scale"),
                 sep = " - ") %>% 
        mutate(data_type = fct_relevel(data_type,
                                       c("ERA5 data", "Station data")),
               spatial_scale = fct_relevel(spatial_scale,
                                           c("Full Richmond MSA",
                                             "Clusters",
                                             "Counties",
                                             "All ZCTAs",
                                             "Selected ZCTAs"))))

saveRDS(wdf_plot_rr,
        here("data/for_manuscript/RR_heat_cold.RDS"))

## plot #-------------------------
(facet_label <- setNames(c("Extreme low temperature", "Extreme high temperature"),
                        c("Cold", "Heat")))

fig <- wdf_plot_rr %>%
        ggplot(aes(y = fct_rev(spatial_scale),
                   x = rr)) +
        geom_pointrange(aes(xmin = rr_lower_ci,
                            xmax = rr_upper_ci,
                            color = data_type,
                            shape = data_type),
                        position = position_dodge(0.45)) +
        facet_wrap(~group,
                   scales = "free_x",
                   labeller = labeller(group = facet_label)) +
        scale_color_manual(values = c("#EC4E49FF", "#5D9D52FF")) +
        labs(y = NULL,
             x = "\nRelative Risk",
             color = NULL,
             shape = NULL) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "top",
              legend.key.width = unit(1.5, "cm"),
              legend.text = element_text(size = 9.5),
              axis.line = ggplot2::element_line(colour = "black"), 
              axis.text = ggplot2::element_text(colour = "black"))


## save as .pdf #---------------------
ggsave_elsevier(here("results/figures/fig_heat_cold.pdf"),
                fig,
                width = "full_page",
                height = 100)


