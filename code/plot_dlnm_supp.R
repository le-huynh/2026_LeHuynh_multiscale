#---
# plot_dlnm_supp.R
#
# This Rscript: generate diagnostic plots for sensitivity analysis of DLNMs
# * shown only the best-performing models within each lag period
#
# Dependencies...
# data/for_manuscript/supp_dlnm.RDS
#
# Produces...
# results/figures/fig_dlnm_supp.pdf
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        chva.extras,     # supplementary functions
        mgcv,
        dlnm,
        splines
)

# data #---------------------------
data <- rio::import(here("data/for_manuscript/supp_dlnm.RDS")) %>%
        tibble()

wdf_plot <- data %>%
        mutate(REML = map_dbl(model, ~.x$gcv.ubre),
               title = paste0("Lag ", lag, ";",
                              " Exposure:", arg_var, "(df=", df_var, ");",
                              " Lag:", arg_lag, "(df=", df_lag, ")")) %>% 
        group_by(lag) %>% 
        slice_min(REML) %>% 
        ungroup() %>% 
        arrange(lag)

ls_plot <- wdf_plot %>% 
        select(title, cp.temp) %>% 
        deframe()

(path <- here("results/figures/"))

(file_name <- wdf_plot %>% 
        select(lag:df_lag) %>% 
        unite("name", lag:df_lag) %>% 
        pull(name))

names(ls_plot)

# plot #----------------------------------
## overall plot #---------------------------

for (i in seq_along(ls_plot)) {
        
        png(file = paste0(path, "/dlnm_overall_lag", file_name[[i]], ".png"),
            width = 4.5,
            height = 3.5,
            units = "in",
            res = 300)

        par(mar = c(3, 3, 2, 0.5),
            mgp = c(1.5, 0.5, 0),
            cex.axis = 0.8,   # axis tick labels
            cex.lab  = 0.8,   # axis titles
            cex.main = 0.8)
        
        plot(ls_plot[[i]],
             "overall",
             ylab = "Relative Risk",
             xlab = "Temperature (°C)",
             xlim = c(-5, 40),
             ylim = c(0.6, 1.3),
             lwd = 1.5,
             main = names(ls_plot)[[i]])
        
        abline(v = 21, lty = 2, col = grey(0.5))

        dev.off()
}

## contour plot #---------------------------
for (i in seq_along(ls_plot)) {
        
        png(file = paste0(path, "/dlnm_contour_lag", file_name[[i]], ".png"),
            width = 4.5,
            height = 3.5,
            units = "in",
            res = 300)
        
        par(mar = c(3, 3, 2, 0),
            mgp = c(1.5, 0.5, 0),
            cex.axis = 0.8,   # axis tick labels
            cex.lab  = 0.8,   # axis titles
            cex.main = 0.8)
        
        plot_contour_dlnm(ls_plot[[i]],
                          z_range = c(0.7, 1.2),
                          nlevels = 30,
                          key.title = title("RR"),
                          plot.title = title(names(ls_plot)[[i]],
                                             xlab = "Temperature (°C)",
                                             ylab = "Lag (days)"))
        
        dev.off()
}

