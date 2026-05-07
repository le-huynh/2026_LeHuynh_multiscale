#---
# get_data_EDvisit_cluster.R
#
# This Rscript: for each cluster
# * generate EDvisit counts per cluster (from counts per county)
# * save the cleaned data for each cluster in a list (.rds)
#
# Dependencies...
# data/process/data_health_era5_county.rds
# data/process/sf_svi_cluster_geometry.rda
#
# Produces...
# data/process/data_EDvisit_cluster.rds
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        chva.extras,    # supplementary functions
        sf
)

# data #-----------------------
(df_cluster <- rio::import(here("data/process/sf_svi_cluster_geometry.rda")) %>% 
        select(county, cluster) %>% 
        arrange(cluster) %>% 
        mutate(cluster = paste0("cluster", cluster)))

data <- rio::import(here("data/process/data_health_era5_county.rds"))

# data cleaning #------------------
(df_cleaned <- data %>% 
        map(\(data) data %>% 
                    filter(!year %in% c(2015, 2023)) %>% 
                    select(date, EDvisit)) %>% 
        enframe(name = "county", value = "data") %>% 
        left_join(df_cluster,
                  by = join_by(county)) %>% 
        nest(.by = cluster) %>% 
        mutate(EDvisit_cluster = map(data,
                              \(df) df %>% 
                                      unnest(data) %>% 
                                      nest(.by = date) %>%
                                      mutate(EDvisit = map_dbl(data,
                                                               \(data) data %>% 
                                                                       pull(EDvisit) %>% 
                                                                       sum(na.rm = TRUE))) %>% 
                                      select(-data))) %>% 
        arrange(cluster))

(df_final <- df_cleaned %>% 
        select(-data) %>% 
        deframe())

# save as .rds
rio::export(df_final, here("data/process/data_EDvisit_cluster.rds"))

