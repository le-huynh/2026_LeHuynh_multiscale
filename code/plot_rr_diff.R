#---
# plot_rr_diff.R
#
# This Rscript: generate a plot to compare differences in relative risk:
# - between the ZCTA-specific model (station data or ERA5 data) and 
#       the full Richmond MSA model (based on station data)
# - at the 1st (cold) and 99th (heat) temperature percentiles
#
# Dependencies...
# code/fn_cal_relative_risk.R
# data/process/data_health_station_zcta.rds
# data/RDSmodel/gam_health_station.rds
# data/RDSmodel/gam_health_station_zcta.rds
# data/RDSmodel/gam_health_era5_zcta.rds
#
# Produces...
# results/figures/fig_rr_diff.pdf
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        lehuynh,
        mgcv,
        dlnm,
        scales,
        ggbreak,
        patchwork,
        cowplot
)

source(here("code/fn_cal_relative_risk.R"))

# function #---------------------------
fn_plot <- function(data,
                    color,
                    subtitle){
        
        fig <- data %>% 
                ggplot(aes(x = EDvisit_mean)) +
                geom_linerange(aes(ymin = diff_lower_ci,
                                   ymax = diff_upper_ci), 
                               color = 'grey30') +
                geom_point(aes(y = diff_rr,
                               fill = data_type),
                           color = "grey95",
                           shape = 21) +
                geom_hline(yintercept = 0,
                           linetype = "dashed",
                           color = "grey10",
                           linewidth = 0.3) +
                scale_fill_manual(values = color) +
                scale_x_continuous(breaks = seq(0, 70, by = 10)) +
                guides(fill = guide_legend(override.aes = list(size = 5))) +
                labs(x = "ED visit mean (visits)",
                     y = "Difference in Relative Risk",
                     subtitle = subtitle,
                     fill = NULL) +
                lehuynh_theme(plot.subtitle = element_text(hjust = 0.1),
                              legend.text = element_text(size = 11))
        
        return(fig)
}

# data #-----------------------------
## EDvisit mean
ls_data <- readRDS(here("data/process/data_health_station_zcta.rds"))

(df_EDvisit_mean <- ls_data %>%
                enframe(name = "zcta") %>%
                mutate(EDvisit_mean = map_dbl(value,
                                              \(data) data %>% 
                                                      filter(!year %in% c(2015, 2023)) %>% 
                                                      pull(EDvisit) %>% 
                                                      mean(na.rm = TRUE))) %>% 
                select(-value))

## location-specific GAM results #----------
# station data
mod_station_full <- readRDS(here("data/RDSmodel/gam_health_station.rds"))
mod_station_zcta_all <- readRDS(here("data/RDSmodel/gam_health_station_zcta.rds"))

# ERA5 data
mod_era5_zcta_all <- readRDS(here("data/RDSmodel/gam_health_era5_zcta.rds"))

## 1st and 99th temperature percentiles #--------------
ref_temp <- 21

(temp_range <- mod_station_full$model %>%
        pull(temp_maxC) %>%
        quantile(probs = c(0.01, 0.99)))

## RR from location-specific models #-----------------------
# station data
(rr_station_full <- cal_relative_risk(model = mod_station_full,
                                     ref_temp = ref_temp,
                                     temp_range = temp_range) %>% 
         select(temp_maxC,
                rr,
                rr_lower_ci,
                rr_upper_ci))

rr_station_zcta_all <- map(.x = mod_station_zcta_all,
                           ~ cal_relative_risk(model = .x,
                                               ref_temp = ref_temp,
                                               temp_range = temp_range))

# ERA5 data
rr_era5_zcta_all <- map(.x = mod_era5_zcta_all,
                           ~ cal_relative_risk(model = .x,
                                               ref_temp = ref_temp,
                                               temp_range = temp_range))

## data for plotting #---------------------- 
(df_msa_cold <- rr_station_full %>% 
        filter(temp_maxC == temp_range[[1]]))

(df_msa_heat <- rr_station_full %>% 
                filter(temp_maxC == temp_range[[2]]))

wls <- map2(.x = list(rr_station_zcta_all,
                      rr_era5_zcta_all),
            .y = c("Station data",
                   "ERA5 data"),
     \(data, data_type) data %>% 
             enframe(name = "zcta") %>% 
             mutate(data_type = data_type,
                    cold = map(value, 
                               \(data) data %>% 
                                       filter(temp_maxC == df_msa_cold$temp_maxC) %>% 
                                       mutate(group = "Cold",
                                              diff_rr = rr - df_msa_cold$rr,
                                              diff_lower_ci = rr_lower_ci - df_msa_cold$rr_lower_ci,
                                              diff_upper_ci = rr_upper_ci - df_msa_cold$rr_upper_ci) %>% 
                                       select(group,
                                              contains("diff"))),
                    heat = map(value, 
                               \(data) data %>% 
                                       filter(temp_maxC == df_msa_heat$temp_maxC) %>% 
                                       mutate(group = "Heat",
                                              diff_rr = rr - df_msa_heat$rr,
                                              diff_lower_ci = rr_lower_ci - df_msa_heat$rr_lower_ci,
                                              diff_upper_ci = rr_upper_ci - df_msa_heat$rr_upper_ci) %>% 
                                       select(group,
                                              contains("diff"))))
     ) %>% 
        list_rbind() %>% 
        left_join(df_EDvisit_mean,
                  by = join_by(zcta)) %>% 
        select(-value)

(wdf_cold <- wls %>% 
        select(zcta,
               EDvisit_mean,
               data_type,
               cold) %>% 
        unnest(cold))

(wdf_heat <- wls %>% 
                select(zcta,
                       EDvisit_mean,
                       data_type,
                       heat) %>% 
                unnest(heat))

(wdf_plot <- bind_rows(wdf_cold, wdf_heat) %>% 
        mutate(data_type = fct_relevel(data_type,
                                       c("ERA5 data",
                                         "Station data")),
               label = paste(data_type, "-", group),
               label = fct_relevel(label,
                                   c("Station data - Cold",
                                     "ERA5 data - Cold",
                                     "Station data - Heat",
                                     "ERA5 data - Heat")),
               .after = group))

# plot #-------------------
fig_station_cold <- wdf_plot %>% 
        filter(label == "Station data - Cold") %>%
        filter(diff_upper_ci < 9) %>% 
        fn_plot(color = "#5D9D52FF",
                subtitle = "Station data - Extreme low temperature") +
        ggbreak::scale_y_break(c(1.2, 8.7),
                               ticklabels = c(8.7))

fig_era5_cold <- wdf_plot %>% 
        filter(label == "ERA5 data - Cold") %>%
        filter(diff_upper_ci < 7.5) %>% 
        fn_plot(color = "#EC4E49FF",
                subtitle = "ERA5 data - Extreme low temperature") +
        ggbreak::scale_y_break(c(1.55, 7),
                               ticklabels = c(7))
fig_station_heat <- wdf_plot %>% 
        filter(label == "Station data - Heat") %>%
        filter(diff_upper_ci < 12) %>% 
        fn_plot(color = "#5D9D52FF",
                subtitle = "Station data – Extreme high temperature") +
        ggbreak::scale_y_break(c(1.15, 11.1),
                               ticklabels = c(11.1))

fig_era5_heat <- wdf_plot %>% 
        filter(label == "ERA5 data - Heat") %>%
        filter(diff_upper_ci < 13) %>% 
        fn_plot(color = "#EC4E49FF",
                subtitle = "ERA5 data - Extreme high temperature") +
        ggbreak::scale_y_break(c(1.5, 12),
                               ticklabels = c(12)) +
        theme(axis.text.x.top = element_blank(), 
              axis.ticks.x.top = element_blank(), 
              axis.line.x.top = element_blank())

# combine plots #----------------------------------
clean_theme <- theme(legend.position = "none",
                     axis.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.line.x.top = element_blank())

fig_station_cold_clean <- fig_station_cold + clean_theme
fig_era5_cold_clean <- fig_era5_cold + clean_theme
fig_station_heat_clean <- fig_station_heat + clean_theme
fig_era5_heat_clean <- fig_era5_heat +
        theme(legend.position = "none",
              axis.title.y = element_blank())

yaxis <- wrap_elements(grid::textGrob('Difference in Relative Risk', 
                                      x = 0.8,
                                      rot = 90))

legend_station <- ggdraw() +
        draw_grob(get_legend(fig_station_cold))

legend_era5 <- ggdraw() +
        draw_grob(get_legend(fig_era5_cold))

legend <- plot_spacer() + legend_era5 + legend_station + plot_spacer() +
        plot_layout(nrow = 1)

p1 <- fig_station_cold_clean +
        fig_era5_cold_clean + 
        plot_layout(nrow = 2)

p2 <- fig_station_heat_clean +
        fig_era5_heat_clean +
        plot_layout(nrow = 2,
                    heights = c(0.9, 1.1))

p1_p2_legend <- (legend / p1 / p2) +
        plot_layout(ncol = 1,
                    heights = c(0.5, 8, 9))

fig <- yaxis + 
        p1_p2_legend +
        plot_layout(ncol = 2,
                    widths = c(0.5, 10)) &
        theme(plot.margin = margin(0, 0, 0, 0),
              panel.spacing = unit(0, "pt"))

# save as .pdf #---------------------
ggsave_elsevier(here("results/figures/fig_rr_diff.pdf"),
                fig,
                width = "full_page",
                height = 240)

