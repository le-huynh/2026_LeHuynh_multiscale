#---
# plot_study_location.R
#
# This Rscript: generate plot for study location at multiple scales
#
# Dependencies...
# data/process/sf_svi_cluster_geometry.rda
#
# Produces...
# results/figures/fig_study_location.png
# results/figures/fig_study_location.pdf
#---

pacman::p_load(
        rio,            # import and export files
        here,           # locate files 
        tidyverse,      # data management and visualization
        sf,
        ggpubr,
        cowplot,        # combine plots
        ggspatial,
        lehuynh,        # save plot with plot size follow journal instructions
        chva.extras     # supplementary functions
)

# data #-----------------------------
# county geometry
(df_cluster <- rio::import(here("data/process/sf_svi_cluster_geometry.rda")) %>% 
         st_as_sf(crs = "WGS84") %>% 
         select(county, cluster, geometry) %>%
         arrange(cluster))

# zipcodes from U.S. Census TIGER/Line Shapefiles (ZCTAs – ZIP Code Tabulation Areas)
(sf_ZCTAs <- chva.extras::sf_va_zcta %>% 
                st_as_sf(crs = "WGS84") %>% 
                filter(zcta_county %in% df_cluster$county,
                       ZCTA5CE20 != "23030"))

# RIC station #-----------------
(df_RIC_station <- data.frame(RIC_longitude = -77.3234,
                              RIC_latitude = 37.5115,
                              label = "RIC weather station"))

RIC_station <- geom_point(data = df_RIC_station,
                          aes(x = RIC_longitude,
                              y = RIC_latitude),
                          shape = 25,
                          size = 1.5,
                          color = "firebrick",
                          fill = "red")

theme_bottom <- theme(axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid = element_blank(),
                      plot.title = element_text(size = 10),
                      plot.caption = element_text(size = 3))

# map of Virginia and Richmond MSA #------------------
fig_VA <- ggplot() +
        geom_sf(data = chva.extras::sf_va_county,
                aes(geometry = geometry),
                fill = NA,
                color = "grey30") +
        geom_sf(data = df_cluster,
                aes(geometry = geometry),
                fill = "#79B7D5FF",
                color = "black") +
        RIC_station +
        # add scale bar
        ggspatial::annotation_scale(location = "bl",
                                    width_hint = 0.2,
                                    style = "ticks",
                                    pad_x = unit(0.85, "cm"),
                                    pad_y = unit(0.1, "cm"),
                                    line_col = "grey30",
                                    text_cex = 0.6) +
        # add north arrow
        ggspatial::annotation_north_arrow(location = "tr",
                                          which_north = "true",
                                          height = unit(0.95, "cm"),
                                          width = unit(0.95, "cm"),
                                          style = ggspatial::north_arrow_fancy_orienteering) +
        labs(#title = "Richmond Metropolitan Statistical Area (MSA), Virginia",
             x = NULL,
             y = NULL) +
        theme_void() +
        theme(plot.margin = margin(0, 0, -18, 0))

# map ZCTAs #------------------------
fig_zcta <- ggplot() + 
        geom_sf(data = sf_ZCTAs,
                aes(geometry = geometry),
                color = "grey30",
                fill = "#9ED5ECFF") +
        # add RIC station
        geom_point(data = df_RIC_station,
                   aes(x = RIC_longitude,
                       y = RIC_latitude,
                       fill = label),
                   shape = 25,
                   size = 1.5,
                   color = "firebrick") +
        # add scale bar
        ggspatial::annotation_scale(location = "bl",
                                    width_hint = 0.2,
                                    style = "ticks",
                                    line_col = "grey30",
                                    text_cex = 0.6) +
        labs(title = "(b) Richmond MSA - 97 ZCTAs",
             x = NULL,
             y = NULL,
             fill = NULL) +
        theme_bw() +
        theme_bottom +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 9),
              legend.key.spacing = unit(0.01, "cm"))

# map county #------------------------
df_cluster1 <- df_cluster %>%
        select(-cluster) %>% 
        mutate(county_label = str_remove(county, "\\s+County$"),
               county_label = case_when(county_label == "Colonial Heights City" ~ "1",
                                        county_label == "Hopewell City" ~ "2",
                                        county_label == "Petersburg City" ~ "3",
                                        county_label == "Richmond City" ~ "4",
                                        TRUE ~ county_label),
               centroid = st_centroid(geometry),
               county_x_adj = st_coordinates(centroid)[,1],
               county_y_adj = st_coordinates(centroid)[,2],
               x_adj = case_when(county_label == "Henrico" ~ county_x_adj + 0.1,
                                 county_label == "King and Queen" ~ county_x_adj + 0.1,
                                 county_label == "King William" ~ county_x_adj - 0.1,
                                 county_label == "Prince George" ~ county_x_adj + 0.04,
                                 TRUE ~ county_x_adj),
               y_adj = case_when(county_label == "Henrico" ~ county_y_adj - 0.07,
                                 county_label == "King and Queen" ~ county_y_adj + 0.03,
                                 county_label == "King William" ~ county_y_adj + 0.1,
                                 county_label == "Prince George" ~ county_y_adj + 0.045,
                                 TRUE ~ county_y_adj)) %>%
        select(county_label, x_adj, y_adj) %>%
        st_drop_geometry() %>% 
        st_as_sf(coords = c("x_adj", "y_adj"), crs = "WGS84")

caption <- "(1) Colonial Heights City; (2) Hopewell City;\n(3) Petersburg City; (4) Richmond City"

fig_county <- ggplot() +
        geom_sf(data = df_cluster,
                aes(geometry = geometry),
                color = "grey30",
                fill = "#9ED5ECFF") +
        geom_sf_text(data = df_cluster1,
                     aes(geometry = geometry,
                         label = county_label),
                     size = 1.75) +
        RIC_station +
        # add scale bar
        ggspatial::annotation_scale(location = "bl",
                                    width_hint = 0.2,
                                    style = "ticks",
                                    line_col = "grey30",
                                    text_cex = 0.6) +
        labs(title = "(c) 13 counties, 4 independent cities",
             x = NULL,
             y = NULL) +
        theme_bw() +
        theme_bottom

caption_county <- ggdraw() +
        draw_label(caption,
                   x = 1,
                   hjust = 1.05,
                   size = 8)

# map cluster #------------------------
fig_cluster <- df_cluster %>% 
        mutate(cluster_label = case_when(cluster == 2 ~ "High",
                                         cluster == 1 ~ "Moderate",
                                         cluster == 3 ~ "Low"),
               cluster_label = fct_relevel(cluster_label,
                                           "High",
                                           "Moderate",
                                           "Low"),
               .after = cluster) %>% 
        ggplot() +
        geom_sf(aes(geometry = geometry,
                    fill = cluster_label),
                color = "grey10") +
        RIC_station +
        # add scale bar
        ggspatial::annotation_scale(location = "bl",
                                    width_hint = 0.2,
                                    style = "ticks",
                                    line_col = "grey30",
                                    text_cex = 0.6) +
        scale_fill_manual(values = c("High" = "#12567EFF",
                                     "Moderate" = "#5096BBFF",
                                     "Low" = "#9ED5ECFF")) +
        labs(x = NULL,
             y = NULL,
             fill = "Vulnerabiity",
             title = "(d) SVI-based geographical clusters") +
        theme_bw() +
        theme_bottom +
        theme(legend.position = "bottom",
              legend.key.size = unit(0.3, 'cm'),
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 8))

# combine figs #-------------------
fig_bottom <- plot_grid(fig_zcta + theme(legend.position = "none"),
                        fig_county,
                        fig_cluster + theme(legend.position = "none"),
                        nrow = 1,
                        align = "h") +
        theme(plot.margin = margin(0, 0, -13, 0))

fig_legend <- plot_grid(get_legend(fig_zcta),
                        caption_county,
                        get_legend(fig_cluster),
                        nrow = 1) +
        theme(plot.margin = margin(-13, 0, 0, 0))

fig <- plot_grid(fig_VA,
                 fig_bottom,
                 fig_legend,
                 ncol = 1,
                 align = "v",
                 labels = c("(a)", ""),
                 label_size = 10,
                 label_fontface = "plain",
                 rel_heights = c(2, 2, 0.1))

# save plot #---------------
ggsave_elsevier(here("results/figures/fig_study_location.pdf"),
                fig,
                width = "full_page",
                height = 170)

