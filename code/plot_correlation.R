#---
# plot_correlation.R
#
# This Rscript: generate plots for correlation between the distribution of
#       weather station and ERA5 data
# * for manuscript: 
#       - full MSA,
#       - all clusters,
#       - ZCTAs, counties with EDvisit mean at 5th, 25th, 75th, 95th quantile
# * for supplementary materials: all ZCTAs, all counties/cities
#
# Dependencies...
# data/process/data_health_station.csv
# data/process/data_health_era5_ric_msa.csv
# data/process/data_health_era5_cluster.rds
# data/process/data_health_era5_county.rds
# data/process/data_health_era5_zcta.rds
#
# Produces...
# results/figures/fig_correlation.pdf
# results/figures/fig_correlation_county.pdf
# results/figures/fig_correlation_zcta1.pdf
# results/figures/fig_correlation_zcta2.pdf
# results/figures/fig_correlation_zcta3.pdf
# results/figures/fig_correlation_zcta4.pdf
# data/for_manuscript/df_correlation.csv
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        lehuynh         # save plot with plot size follow journal instructions
)

# function #---------------------------
fn_ls_working <- function(list){
  
  res <- list %>% 
    imap(\(data, idx) data %>%
         filter(!year %in% c(2015, 2023)) %>% 
         select(date, era5 = temp_maxC) %>% 
         left_join(df_station,
                   by = join_by(date)) %>% 
         mutate(location = idx))
  
  return(res)
}

fn_df_cor <- function(ls_working){
  
  res <- ls_working %>% 
    enframe(name = "location") %>%
    mutate(cor = map_dbl(value, 
                         \(data) data %>% 
                           summarise(cor = cor(era5, station)) %>% 
                           pull(cor))) %>% 
    mutate(cor_label = paste("r =", round(cor, digits = 4)),
           location = as_factor(location)) %>% 
    select(location, cor_label)
  
  return(res)
}

fn_plot <- function(df_working,
                    df_cor,
                    nrow_facet = NULL,
                    x_cor = 5,
                    y_cor = 0.04){
  
  fig <- ggplot() +
    geom_density(data = df_working,
                 aes(x = temp_maxC,
                     colour = data_type,
                     fill = data_type),
                 alpha = 0.2,
                 linewidth = 0.6) +
    facet_wrap(~location, nrow = nrow_facet) +
    geom_text(data = df_cor,
              aes(x = x_cor, y = y_cor, label = cor_label)) +
    labs(x = "\nTemperature (°C)",
         y = "Density\n",
         color = NULL,
         fill = NULL) +
    scale_fill_manual(labels = c("ERA5 data", "Station data"),
                      values = c("#BA4000", "#00BA38")) +
    scale_color_manual(labels = c("ERA5 data", "Station data"),
                       values = c("#BA4000", "#00BA38")) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid = element_blank())
  
  return(fig)
}

fn_plot_full <- function(ls_data,
                         nrow_facet,
                         x_cor = 5,
                         y_cor = 0.04){
  
  ls_working <- ls_data %>%
    fn_ls_working()
  
  df_cor <- ls_working %>%
      fn_df_cor() %>% 
      mutate(location = as_factor(location))
  
  fig <- ls_working %>% 
    map(\(data) data %>% 
          pivot_longer(c(era5, station),
                       names_to = "data_type",
                       values_to = "temp_maxC")) %>% 
    list_rbind() %>%
    mutate(location = as_factor(location)) %>%
    fn_plot(df_cor = df_cor,
            nrow_facet = nrow_facet,
            x_cor = x_cor,
            y_cor = y_cor)
  
}
# data #--------------------------------
(df_station <- rio::import(here("data/process/data_health_station.csv")) %>% 
        as_tibble() %>% 
        filter(!year %in% c(2015, 2023)) %>% 
        mutate(date = date(date)) %>%
        select(date, station = temp_maxC))

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

# selected county with EDvisit mean at 5th, 25th, 75th, 95th quantile
ls_selected_county <- ls_era5_county %>% 
  enframe(name = "county") %>% 
  mutate(meanED = map_dbl(value,
                          \(data) data %>% 
                            summarise(mean = mean(EDvisit, na.rm = TRUE)) %>% 
                            pull(mean))) %>% 
  arrange(meanED) %>% 
  rowid_to_column() %>% 
  filter(rowid %in% round(c(0.05, 0.25, 0.75, 0.95) * length(ls_era5_county))) %>% 
  select(county, value) %>% 
  deframe()

# selected ZCTA with EDvisit mean at 5th, 25th, 75th, 95th quantile
ls_selected_zcta <- ls_era5_zcta %>% 
  enframe(name = "zcta") %>% 
  mutate(meanED = map_dbl(value,
                          \(data) data %>% 
                            summarise(mean = mean(EDvisit, na.rm = TRUE)) %>% 
                            pull(mean))) %>% 
  arrange(meanED) %>% 
  rowid_to_column() %>% 
  filter(rowid %in% round(c(0.05, 0.25, 0.75, 0.95) * length(ls_era5_zcta))) %>% 
  select(zcta, value) %>% 
  deframe()

# plot - manuscript #--------------------------
ls_working <- c(list(msa = df_era5_msa),
          ls_era5_cluster,
          ls_selected_county,
          ls_selected_zcta) %>%
  fn_ls_working()

(df_cor <- ls_working %>%
    fn_df_cor() %>% 
    mutate(location = case_when(location == "msa" ~ "Full Richmond MSA",
                                location == "cluster1" ~ "High vulnerability cluster",
                                location == "cluster2" ~ "Moderate vulnerability cluster",
                                location == "cluster3" ~ "Low vulnerability cluster",
                                !str_detect(location, "[:alpha:]") ~ paste("ZCTA", location),
                                TRUE ~ location),
           location = as_factor(location)))

fig <- ls_working %>% 
  map(\(data) data %>% 
        pivot_longer(c(era5, station),
                     names_to = "data_type",
                     values_to = "temp_maxC")) %>% 
  list_rbind() %>%
  mutate(location = case_when(location == "msa" ~ "Full Richmond MSA",
                              location == "cluster1" ~ "High vulnerability cluster",
                              location == "cluster2" ~ "Moderate vulnerability cluster",
                              location == "cluster3" ~ "Low vulnerability cluster",
                              !str_detect(location, "[:alpha:]") ~ paste("ZCTA", location),
                              TRUE ~ location),
         location = as_factor(location)) %>%
  fn_plot(df_cor = df_cor)

# save as .pdf
ggsave_elsevier(here("results/figures/fig_correlation.pdf"),
                fig,
                width = "full_page",
                height = 240/3*2)

# plot - all counties #----------------
fig_county <- fn_plot_full(ls_data = ls_era5_county,
                             nrow_facet = 5)

# save as .pdf
ggsave_elsevier(here("results/figures/fig_correlation_county.pdf"),
                fig_county,
                width = "full_page",
                height = 240)

# plot - all zctas #----------------
## ZCTA 1-25 #---------------
fig_zcta1 <- fn_plot_full(ls_data = ls_era5_zcta %>% keep_at(at = 1:25),
                          nrow_facet = 5,
                          x_cor = 10)

# save as .pdf
ggsave_elsevier(here("results/figures/fig_correlation_zcta1.pdf"),
                fig_zcta1,
                width = "full_page",
                height = 240)

## ZCTA 26-50 #---------------
fig_zcta2 <- fn_plot_full(ls_data = ls_era5_zcta %>% keep_at(at = 26:50),
                          nrow_facet = 5,
                          x_cor = 10)

# save as .pdf
ggsave_elsevier(here("results/figures/fig_correlation_zcta2.pdf"),
                fig_zcta2,
                width = "full_page",
                height = 240)

## ZCTA 51-75 #---------------
fig_zcta3 <- fn_plot_full(ls_data = ls_era5_zcta %>% keep_at(at = 51:75),
                          nrow_facet = 5,
                          x_cor = 10)

# save as .pdf
ggsave_elsevier(here("results/figures/fig_correlation_zcta3.pdf"),
                fig_zcta3,
                width = "full_page",
                height = 240)

## ZCTA 76-97 #---------------
fig_zcta4 <- fn_plot_full(ls_data = ls_era5_zcta %>% keep_at(at = 76:97),
                          nrow_facet = 5,
                          x_cor = 10)

# save as .pdf
ggsave_elsevier(here("results/figures/fig_correlation_zcta4.pdf"),
                fig_zcta4,
                width = "full_page",
                height = 240)

# correlation data #---------------
df_cor_working <- c(list(msa = df_era5_msa),
                ls_era5_cluster,
                ls_era5_county,
                ls_era5_zcta) %>%
  fn_ls_working() %>%
  enframe(name = "location") %>%
  mutate(cor = map_dbl(value, 
                       \(data) data %>% 
                         summarise(cor = cor(era5, station)) %>% 
                         pull(cor))) %>% 
  select(location, cor)

rio::export(df_cor_working,
            here("data/for_manuscript/df_correlation.csv"))

