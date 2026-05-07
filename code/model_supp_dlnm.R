#---
# model_supp_dlnm.R
#
# This Rscript: generate sensitivity analysis results for DLNM test
# * test cross-basis; 
# * generate cross-pred of temperature
#
# Dependencies...
# data/process/data_health_station.csv
#
# Produces...
# data/for_manuscript/supp_dlnm.RDS
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        mgcv,
        dlnm,
        splines,
        tictoc
)

# data #------------------------------
(data <- rio::import(here("data/process/data_health_station.csv")) %>% 
         tibble())

(wdf <- data %>% 
                filter(!year %in% c(2015, 2023)) %>% 
                rowid_to_column(var = "trend"))

# `bs()`: 'df' 3 was too small; have used 4
lag_val <- c(3, 7, 14, 21)
arg_fun <- c("ns", "bs")
df_val <- c(4, 5, 6)

# model fitting #-------------------------
tic()
model_cbtemp <- expand.grid(
        lag = lag_val,
        arg_var = arg_fun,
        df_var = df_val,
        arg_lag = arg_fun,
        df_lag = df_val,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE) %>% 
        tibble() %>% 
        mutate(cb.temp = pmap(list(lag, arg_var, df_var, arg_lag, df_lag),
                              \(lag, arg_var, df_var, arg_lag, df_lag){
                                      cb.temp <- crossbasis(wdf$temp_maxC,
                                                            # lagged effect up to X days
                                                            lag = lag,
                                                            # effects of temperature
                                                            argvar = list(fun = arg_var, df = df_var),
                                                            # lag effects
                                                            arglag = list(fun = arg_lag, df = df_lag))
                                      return(cb.temp)
                              }),
               model = map(cb.temp,
                           \(cb.temp){
                                   mod <- gam(EDvisit ~ cb.temp +
                                                      s(trend, k = 7 * 4) + 
                                                      s(DTR, k = 4) +
                                                      as.factor(day_of_week) +
                                                      as.factor(holiday_flag),
                                              family = "quasipoisson",
                                              method = "REML",
                                              data = wdf)
                                   return(mod)
                           }),
               cp.temp = map2(.x = cb.temp,
                              .y = model,
                              \(cb.temp, model){
                                      res <- crosspred(cb.temp,
                                                       model,
                                                       by = 0.2,
                                                       bylag = 0.2,
                                                       cen = 21)
                                      return(res)
                              }
               ))
toc()

# save as .RDS #------------------------
saveRDS(model_cbtemp,
        here("data/for_manuscript/supp_dlnm.RDS"))

