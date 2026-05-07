#---
# plot_relative_risk.R
#
# This Rscript: generate plot to compare relative risk curves
#
# Dependencies...
# code/fn_cal_relative_risk.R
# data/RDSmodel/gam_health_station.rds
# data/RDSmodel/gam_health_station_cluster.rds
# data/RDSmodel/gam_health_station_county.rds
# data/RDSmodel/gam_health_station_county_excluding_charles_city.rds
# data/RDSmodel/gam_health_station_zcta.rds
# data/RDSmodel/gam_health_station_zcta_mean5.rds
# data/RDSmodel/gam_health_era5.rds
# data/RDSmodel/gam_health_era5_cluster.rds
# data/RDSmodel/gam_health_era5_county.rds
# data/RDSmodel/gam_health_era5_county_excluding_charles_city.rds
# data/RDSmodel/gam_health_era5_zcta.rds
# data/RDSmodel/gam_health_era5_zcta_mean5.rds
# data/RDSmodel/pgam_meta_station_county.rds
# data/RDSmodel/pgam_meta_station_county_excluding_charles_city.rds
# data/RDSmodel/pgam_meta_station_cluster.rds
# data/RDSmodel/pgam_meta_station_zcta_all.rds
# data/RDSmodel/pgam_meta_station_zcta_mean5.rds
# data/RDSmodel/pgam_meta_era5_county.rds
# data/RDSmodel/pgam_meta_era5_county_excluding_charles_city.rds
# data/RDSmodel/pgam_meta_era5_cluster.rds
# data/RDSmodel/pgam_meta_era5_zcta_all.rds
# data/RDSmodel/pgam_meta_era5_zcta_mean5.rds
#
# Produces...
# results/figures/fig_relative_risk.pdf
# results/figures/fig_relative_risk_supp.pdf
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        lehuynh,
        mgcv,
        dlnm,
        ggh4x,
        patchwork
)

source(here("code/fn_cal_relative_risk.R"))

ref_temp <- 21

# function #----------------------
fn_df_location_specific <- function(ls_data,
                                    data_type){
        
        res <- ls_data %>% 
                enframe() %>% 
                mutate(rr = map(value,
                                \(data) data %>% select(temp_maxC,
                                                        rr,
                                                        rr_lower_ci,
                                                        rr_upper_ci))) %>% 
                select(-value) %>% 
                unnest(rr) %>% 
                mutate(data_type = data_type,
                       model_type = "Location-specific RR")
        
        return(res)
}

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

fn_plot_specific <- function(data_rr,
                             data_meta,
                             data_type_levels,
                             colors = c("#79B7D5FF","#23719CFF")){
        
        fig <- bind_rows(data_rr,
                         data_meta) %>%
                mutate(data_type = fct_relevel(data_type,
                                               data_type_levels)) %>% 
                ggplot(aes(x = temp_maxC,
                           y = rr,
                           group = name)) +
                geom_line(aes(color = as_factor(model_type),
                              linetype = as_factor(model_type),
                              size = as_factor(model_type))) +
                geom_hline(yintercept = 1,
                           linetype = "dashed",
                           color = "grey10",
                           linewidth = 0.3) +
                geom_vline(xintercept = ref_temp,
                           linetype = "dashed",
                           color = "grey10",
                           linewidth = 0.3) +
                facet_wrap(~data_type, ncol = 1) +
                ylim(0.7, 1.25) +
                scale_linetype_manual(values = c("longdash", "solid")) +
                scale_color_manual(values = colors) +
                scale_size_manual(values = c(0.5, 1)) +
                labs(x = "Temperature (°C)",
                     y = "Relative Risk",
                     size = NULL,
                     linetype = NULL,
                     color = NULL) +
                theme_bw() +
                theme(panel.grid = element_blank(),
                      legend.position = "top",
                      legend.key.width = unit(1.5, "cm"),
                      legend.text = element_text(size = 9.5)) +
                guides(linetype = guide_legend(nrow = 1),
                       size = "none",
                       color = "none")
        
        return(fig)
}

fn_plot_meta <- function(data){
        
        fig <- data %>% 
                ggplot(aes(x = temp_maxC)) +
                geom_line(aes(y = rr,
                              colour = as_factor(data_type)),
                          linewidth = 1.2) +
                geom_ribbon(aes(ymin = rr_lower_ci,
                                ymax = rr_upper_ci,
                                fill = as_factor(data_type)),
                            alpha = 0.5) +
                geom_hline(yintercept = 1,
                           linetype = "dashed",
                           color = "grey10",
                           linewidth = 0.3) +
                geom_vline(xintercept = ref_temp,
                           linetype = "dashed",
                           color = "grey10",
                           linewidth = 0.3) +
                facet_wrap(~spatial_scale, ncol = 1) +
                ylim(0.7, 1.25) +
                scale_color_manual(values = c("#7C000CFF", "#186D2EFF")) +
                scale_fill_manual(values = c("#F28278FF", "#7CB271FF")) +
                labs(x = "Temperature (°C)",
                     y = "Relative Risk",
                     color = NULL,
                     fill = NULL) +
                theme_bw() +
                theme(panel.grid = element_blank(),
                      legend.text = element_text(size = 9.5),
                      legend.key.width = unit(1.5, "cm"))
        
        return(fig)
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

## relative risk from location-specific models #-----------------------
# station data
rr_station_full <- cal_relative_risk(model = mod_station_full,
                                     ref_temp = ref_temp)

rr_station_county <- map(.x = mod_station_county,
                         ~ cal_relative_risk(model = .x,
                                             ref_temp = ref_temp))

rr_station_cluster <- map(.x = mod_station_cluster,
                          ~ cal_relative_risk(model = .x,
                                              ref_temp = ref_temp))

rr_station_zcta_all <- map(.x = mod_station_zcta_all,
                           ~ cal_relative_risk(model = .x,
                                               ref_temp = ref_temp))

rr_station_zcta_mean5 <- map(.x = mod_station_zcta_mean5,
                           ~ cal_relative_risk(model = .x,
                                               ref_temp = ref_temp))

# ERA5 data
rr_era5_full <- cal_relative_risk(model = mod_era5_full,
                                     ref_temp = ref_temp)

rr_era5_county <- map(.x = mod_era5_county,
                         ~ cal_relative_risk(model = .x,
                                             ref_temp = ref_temp))

rr_era5_cluster <- map(.x = mod_era5_cluster,
                          ~ cal_relative_risk(model = .x,
                                              ref_temp = ref_temp))

rr_era5_zcta_all <- map(.x = mod_era5_zcta_all,
                           ~ cal_relative_risk(model = .x,
                                               ref_temp = ref_temp))

rr_era5_zcta_mean5 <- map(.x = mod_era5_zcta_mean5,
                        ~ cal_relative_risk(model = .x,
                                            ref_temp = ref_temp))

## relative risk from meta-regression models #------------
# station data
rr_meta_station_county <- readRDS(here("data/RDSmodel/pgam_meta_station_county.rds"))
rr_meta_station_county_ex_charles <- readRDS(here("data/RDSmodel/pgam_meta_station_county_excluding_charles_city.rds"))
rr_meta_station_cluster <- readRDS(here("data/RDSmodel/pgam_meta_station_cluster.rds"))
rr_meta_station_zcta_all <- readRDS(here("data/RDSmodel/pgam_meta_station_zcta_all.rds"))
rr_meta_station_zcta_mean5 <- readRDS(here("data/RDSmodel/pgam_meta_station_zcta_mean5.rds"))

# ERA5 data
rr_meta_era5_county <- readRDS(here("data/RDSmodel/pgam_meta_era5_county.rds"))
rr_meta_era5_county_ex_charles <- readRDS(here("data/RDSmodel/pgam_meta_era5_county_excluding_charles_city.rds"))
rr_meta_era5_cluster <- readRDS(here("data/RDSmodel/pgam_meta_era5_cluster.rds"))
rr_meta_era5_zcta_all <- readRDS(here("data/RDSmodel/pgam_meta_era5_zcta_all.rds"))
rr_meta_era5_zcta_mean5 <- readRDS(here("data/RDSmodel/pgam_meta_era5_zcta_mean5.rds"))

# main-plot #---------------------
## plot -- station-specific RR #----------------------
station_data_type <- c("Station data - Clusters",
                       "Station data - Counties",
                       "Station data - All ZCTAs",
                       "Station data - Selected ZCTAs")

### data - location-specific RR #------------
ls_rr_station <- list(rr_station_cluster,
                      rr_station_county,
                      rr_station_zcta_all,
                      rr_station_zcta_mean5)

(wdf_rr_station <- map2(.x = ls_rr_station,
                       .y = station_data_type,
                        ~ fn_df_location_specific(ls_data = .x,
                                                  data_type = .y)) %>% 
                list_rbind())

### data - pooled RR #--------------
ls_meta_station <- list(cluster = rr_meta_station_cluster,
                        county = rr_meta_station_county,
                        zcta_all = rr_meta_station_zcta_all,
                        zcta_mean5 = rr_meta_station_zcta_mean5)

(wdf_meta_station <- map2(.x = ls_meta_station,
                          .y = station_data_type,
                          ~ fn_df_meta(data = .x, data_type = .y)) %>% 
        list_rbind())

### plot #--------------------
fig_station <- fn_plot_specific(data_rr = wdf_rr_station,
                               data_meta = wdf_meta_station,
                               data_type_levels = station_data_type,
                               colors = c("#7CB271FF","#186D2EFF"))

## plot -- era5-specific RR #----------------------
era5_data_type <- c("ERA5 data - Clusters",
                    "ERA5 data - Counties",
                    "ERA5 data - All ZCTAs",
                    "ERA5 data - Selected ZCTAs")

### data - location-specific RR #------------------
ls_rr_era5 <- list(rr_era5_cluster,
                      rr_era5_county,
                      rr_era5_zcta_all,
                      rr_era5_zcta_mean5)

(wdf_rr_era5 <- map2(.x = ls_rr_era5,
                        .y = era5_data_type,
                        ~ fn_df_location_specific(ls_data = .x,
                                                  data_type = .y)) %>% 
                list_rbind())

### data - pooled RR #--------------------
ls_meta_era5 <- list(cluster = rr_meta_era5_cluster,
                        county = rr_meta_era5_county,
                        zcta_all = rr_meta_era5_zcta_all,
                        zcta_mean5 = rr_meta_era5_zcta_mean5)

(wdf_meta_era5 <- map2(.x = ls_meta_era5,
                          .y = era5_data_type,
                          ~ fn_df_meta(data = .x, data_type = .y)) %>% 
                list_rbind())

### plot #-------------------------------
fig_era5 <- fn_plot_specific(data_rr = wdf_rr_era5,
                                data_meta = wdf_meta_era5,
                                data_type_levels = era5_data_type,
                                colors = c("#F28278FF","#7C000CFF"))

## plot - compare RR with CI #------------------------
### data #----------------
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

(wdf_plot <- bind_rows(wdf_full_msa,
                       wdf_meta_station %>% select(-name, -model_type),
                       wdf_meta_era5 %>% select(-name, -model_type)) %>% 
                separate(data_type,
                         into = c("data_type",
                                  "spatial_scale"),
                         sep = " - ") %>% 
                mutate(data_type = fct_relevel(data_type,
                                               c("ERA5 data", "Station data")),
                       spatial_scale = case_when(spatial_scale != "Full Richmond MSA" ~ paste("Pooled RR -", spatial_scale),
                                                 TRUE ~ spatial_scale),
                       spatial_scale = fct_relevel(spatial_scale,
                                                   c("Full Richmond MSA",
                                                     "Pooled RR - Clusters",
                                                     "Pooled RR - Counties",
                                                     "Pooled RR - All ZCTAs",
                                                     "Pooled RR - Selected ZCTAs"))))

### plot - pooled RR #------------------
fig_meta <- wdf_plot %>% 
        filter(spatial_scale != "Full Richmond MSA") %>% 
        fn_plot_meta() +
        theme(legend.position = "none")

### plot - full MSA #------------------
fig_msa <- wdf_plot %>% 
        filter(spatial_scale == "Full Richmond MSA") %>% 
        fn_plot_meta() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())

## combined plot #---------------------
layout <- '
AXX
BCD
'

fig <- wrap_plots(A = fig_msa,
                  B = fig_meta, 
                  C = fig_station, 
                  D = fig_era5,
                  X = guide_area(),
                  design = layout) +
        plot_layout(axes = "collect",
                    guides = "collect",
                    heights = c(1, 4))

# save as .pdf
ggsave_elsevier(here("results/figures/fig_relative_risk.pdf"),
                fig,
                width = "full_page",
                height = 240)

# supp-plot #--------------------
## plot -- station-specific RR #----------------------
station_data_type <- c("Station data - All counties",
                       "Station data - excluding Charles City")

### data - location-specific RR #------------
ls_rr_station <- list(rr_station_county,
                      rr_station_county %>% 
                              discard_at(at = "Charles City County"))

(wdf_rr_station <- map2(.x = ls_rr_station,
                        .y = station_data_type,
                        ~ fn_df_location_specific(ls_data = .x,
                                                  data_type = .y)) %>% 
                list_rbind())

### data - pooled RR #--------------
ls_meta_station <- list(county_all = rr_meta_station_county,
                        county_ex = rr_meta_station_county_ex_charles)

(wdf_meta_station <- map2(.x = ls_meta_station,
                          .y = station_data_type,
                          ~ fn_df_meta(data = .x, data_type = .y)) %>% 
                list_rbind())

### plot #--------------------
fig_station <- fn_plot_specific(data_rr = wdf_rr_station,
                                data_meta = wdf_meta_station,
                                data_type_levels = station_data_type,
                                colors = c("#7CB271FF","#186D2EFF"))

## plot -- era5-specific RR #----------------------
era5_data_type <- c("ERA5 data - All counties",
                    "ERA5 data - excluding Charles City")

### data - location-specific RR #------------------
ls_rr_era5 <- list(rr_era5_county,
                   rr_era5_county %>% 
                           discard_at(at = "Charles City County"))

(wdf_rr_era5 <- map2(.x = ls_rr_era5,
                     .y = era5_data_type,
                     ~ fn_df_location_specific(ls_data = .x,
                                               data_type = .y)) %>% 
                list_rbind())

### data - pooled RR #--------------------
ls_meta_era5 <- list(county_all = rr_meta_era5_county,
                     county_ex = rr_meta_era5_county_ex_charles)

(wdf_meta_era5 <- map2(.x = ls_meta_era5,
                       .y = era5_data_type,
                       ~ fn_df_meta(data = .x, data_type = .y)) %>% 
                list_rbind())

### plot #-------------------------------
fig_era5 <- fn_plot_specific(data_rr = wdf_rr_era5,
                             data_meta = wdf_meta_era5,
                             data_type_levels = era5_data_type,
                             colors = c("#F28278FF","#7C000CFF"))

## plot - compare RR with CI #------------------------
### data #----------------
(wdf_plot <- 
         bind_rows(wdf_meta_station %>% select(-name, -model_type),
                       wdf_meta_era5 %>% select(-name, -model_type)) %>% 
                separate(data_type,
                         into = c("data_type",
                                  "spatial_scale"),
                         sep = " - ") %>% 
                mutate(data_type = fct_relevel(data_type,
                                               c("ERA5 data", "Station data")),
                       spatial_scale = paste("Pooled RR -", spatial_scale),
                       spatial_scale = fct_relevel(spatial_scale,
                                                   c("Pooled RR - All counties",
                                                     "Pooled RR - excluding Charles City"))))

### plot - pooled RR #------------------
fig_meta <- wdf_plot %>% 
        fn_plot_meta() +
        theme(legend.position = "none")

## combined plot #---------------------
layout <- '
AXX
BCD
'

fig <- wrap_plots(A = fig_msa,
                  B = fig_meta, 
                  C = fig_station, 
                  D = fig_era5,
                  X = guide_area(),
                  design = layout) +
        plot_layout(axes = "collect",
                    guides = "collect",
                    heights = c(1, 2))

# save as .pdf
ggsave_elsevier(here("results/figures/fig_relative_risk_supp.pdf"),
                fig,
                width = "full_page",
                height = 240/4*3)

