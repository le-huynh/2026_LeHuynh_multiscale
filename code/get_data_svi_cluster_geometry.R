#---
# get_data_svi_cluster_geometry.R
#
# This Rscript: from SVI 2022 data - 16 factors
# * perform cluster analysis for Richmond MSA counties 
# * save clustered SVI results as .rda
# Note: https://www.atsdr.cdc.gov/place-health/php/svi/svi-data-documentation-download.html
#
# Dependencies...
# data/raw/SVI_2022_virginia_county.csv
#
# Produces...
# data/for_manuscript/supp_svi_cluster.csv
# data/process/sf_svi_cluster_geometry.rda
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        chva.extras,   # supplementary functions
        factoextra,
        cluster,
        dendextend,
        sf
)

# data #----------------

ric_county <- c("Amelia County",        
                "Charles City County",  
                "Chesterfield County",  
                "Colonial Heights City",
                "Dinwiddie County",     
                "Goochland County",     
                "Hanover County",       
                "Henrico County",       
                "Hopewell City",        
                "King William County",  
                "King and Queen County",
                "New Kent County",      
                "Petersburg City",      
                "Powhatan County",      
                "Prince George County", 
                "Richmond City",        
                "Sussex County" )

(df <- rio::import(here("data/raw/SVI_2022_virginia_county.csv")) %>% 
                tibble() %>% 
                filter(COUNTY %in% ric_county) %>% 
                select(COUNTY,
                       starts_with("EPL")) %>% 
                column_to_rownames(var = "COUNTY"))

# hierarchical clustering #---------------------
res_agnes <- agnes(x = df, # data
                   stand = TRUE,
                   metric = "euclidean", # metric for distance matrix
                   method = "complete" # linkage method
)

(cluster_res <- cutree(res_agnes, k = 3))

# k-means clustering #-----------------
(initial_centers <- df %>% 
         rownames_to_column() %>% 
         left_join(enframe(cluster_res),
                   by = c("rowname" = "name")) %>% 
         group_by(value) %>% 
         summarise(across(starts_with("EPL"), mean)) %>% 
         select(-value) %>% 
         as.matrix())

km.res <- kmeans(df, centers = initial_centers)

# clustering results #---------------
(df_res1 <- df %>%
        rownames_to_column(var = "county") %>% 
        left_join(km.res$cluster %>% enframe(name = "county", value = "cluster"),
                  by = c("county" = "county")))

(df_res2 <- df_res1 %>% 
        left_join(chva.extras::sf_va_county %>% select(county, geometry),
                  by = c("county" = "county")) %>% 
        st_as_sf())

# save as .rda
rio::export(df_res1, here("data/for_manuscript/supp_svi_cluster.csv"))
rio::export(df_res2, here("data/process/sf_svi_cluster_geometry.rda"))

