#---
# table_supp_dlnm.R
#
# This Rscript:
# * generate table for model comparison in Supplementary Materials
# * test of DLNM
#
# Dependencies...
# data/for_manuscript/supp_dlnm.RDS
#
# Produces...
# data/for_manuscript/supp_dlnm_comparison.csv
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        mgcv,
        dlnm,
        splines
)

# data #--------------------------------
data <- rio::import(here("data/for_manuscript/supp_dlnm.RDS")) %>% 
        tibble() %>% 
        select(-cb.temp, -cp.temp)

# generate table #----------------------
(res <- data %>% 
        mutate(REML = map_dbl(model,
                              ~.x$gcv.ubre),
               dev_explained = map_dbl(model,
                                       ~summary(.x)$dev.expl * 100),
               adj_Rsq = map_dbl(model,
                                 ~round(summary(.x)$r.sq, digits = 3)),
               converged = map_lgl(model,
                                   ~.x$converged)) %>% 
         arrange(REML) %>% 
         select(-model))

# save .csv #------------
rio::export(res,
            here("data/for_manuscript/supp_dlnm_comparison.csv"))

