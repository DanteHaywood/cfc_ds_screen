# 04_mapping_graphing ----------------------------------------------------------------------
# Build maps and graphs for presentation.
# Adapted from https://www.r-graph-gallery.com/168-load-a-shape-file-into-r.html
# And https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html
# If having trouble with packages, restart R
# NC Census tract data from US Census 2017:
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2017.html

source("R/init.R")

# Read GeoDB -------------------------------------------------------------------
spdf <- readOGR( 
  dsn= paste0("data/cb_2017_37_tract_500k") , 
  layer="cb_2017_37_tract_500k",
  verbose=FALSE
)

# Region gets the census FIPS key from GeoDB "data"
spdf_fortified <- tidy(spdf, region = "GEOID")
spdf_fortified$id <- as.numeric(spdf_fortified$id)

# Not needed
rm(spdf)


# Join our other data
spdf_fortified <- left_join(spdf_fortified, all_train, 
                            by = c("id" = "select_state_county_tract"))

# Map Food Deserts -------------------------------------------------------------
# Change legend titles
spdf_fortified$fd_2017_label <- factor(ifelse(
  spdf_fortified$food_desert_2017 == 1,
  "Food Desert",
  "Not Food Desert"))

ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes( x = long, y = lat, group = group, 
                    fill = fd_2017_label), 
               size=0) +
  theme_void()+
  labs(title = "USDA Food Desert Designations 2017", 
       subtitle = "By 2017 US Census Tract",
       fill = "USDA Food Desert 2017") + 
  scale_color_hue(labels = c("Not Food Desert", "Food Desert")) +
  theme(
    text = element_text(color = "#22211d"),
    #plot.background = element_rect(fill = "#f5f5f5", color = NA),
    #panel.background = element_rect(fill = "#f5f5f5", color = NA),
    #legend.background = element_rect(fill = "#f5f5f5", color = NA),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47"),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47"),
    legend.position = c(0.1, 0.15)
  ) +
  coord_map() +
  ggsave("output/food_desert_map.png", device = "png", bg = "transparent")

# Map Food Deserts Prediction --------------------------------------------------
# Map is really the same as above given model effectiveness
wrong_pred <- factor(ifelse(spdf_fortified$food_desert_2017 != 
                       spdf_fortified$preds_final,
                     "False Prediction",
                     "Correct Prediction"))

spdf_fortified$wrong_pred <- wrong_pred

ggplot() +
  geom_polygon(data = spdf_fortified,
               aes( x = long, y = lat, group = group, 
                    fill = wrong_pred), 
               size = 0) +
  theme_void()+
  labs(title = "Random Forest (k=50) Prediction Overview", 
       subtitle = "By 2017 US Census Tract") + 
  scale_fill_viridis(discrete = TRUE,
                     #oob = scales::squish_infinite,
                     name="Food Desert Prediction", 
                     #guide = guide_legend( keyheight = unit(3, units = "mm"), 
                    #                       keywidth=unit(12, units = "mm"), 
                    #                       label.position = "bottom", 
                    #                       title.position = 'top', 
                    #                       nrow=1) 
                    ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47"),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47"),
    legend.position = c(0.15, 0.15)
  ) +
  coord_map() +
  ggsave("output/food_desert_prediction_outcome.png", 
         device = "png", bg = "transparent")

# Plot Multivariate Analysis ---------------------------------------------------

all_train$fd_2017_label <- factor(ifelse(
  all_train$food_desert_2017 == 1,
  "Food Desert",
  "Not Food Desert"))

graph_vars <- c("povertyrate", "medianfamilyincome", "pct_hhold_no_veh",
                "log_pop_per_sqmi_est_2018")
labs <- c("Poverty Rate", "Median Family Income", "% Households No Vehicle",
          "Log Population per SqMi. 2018")

for (i in 1:length(graph_vars)) {
  g <- ggplot(all_train, 
              aes(fd_2017_label, .data[[graph_vars[i]]])) +
    geom_boxplot(aes(fill=fd_2017_label)) +
    #geom_violin(aes(fill=fd_2017_label)) +
    labs(fill = "Tract Designation") + 
    xlab("") +
    ylab(labs[i]) +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      text = element_text(color = "#22211d", size = 22),
      plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47"),
      plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47")
    ) + 
    guides(fill=FALSE)
  
  fname <- paste0("output/", graph_vars[i], "_fd_boxplot.png")
  print(g)
  g + ggsave(fname, device = "png", bg = "transparent")
  
}

# Model ROC Curves -------------------------------------------------------------
ggplot(roc_coords_all) +
  geom_line(aes(x = fpr_rf, y = tpr_rf, color = "rf_line"), size = 1.2) +
  geom_line(aes(x = fpr_logi, y = tpr_logi, color = "logi_line"),  size = 1.2) +
  geom_abline(aes(intercept = 0, slope = 1, color = "ref_line"), 
              linetype = "longdash", size = 1, show.legend = FALSE) +
  labs(title = "NC Food Desert Prediction", subtitle = "Model ROC") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  scale_colour_manual(name="ROC AUC",
                      values = c(rf_line = "blue", logi_line="orange",
                                 ref_line = "red"),
                      labels = c(rf_line = rf_legend_text, 
                                 logi_line = logi_legend_text, 
                                 ref_line = rand_legend_text)
  ) +
  theme(legend.position = c(0.80, 0.15),
        text = element_text(color = "#22211d", size = 14),
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47"),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
  ) + 
  ggsave("output/roc_comparison.png", device = "png", bg = "transparent")

# Random Forest Feature Importance ---------------------------------------------
ggplot(rf_importance, aes(importance, reorder(variable, importance))) +
  geom_col() + 
  labs(title = "Random Forest (k = 50) Feature Importance", 
       subtitle = "(Larger is more important)") +
  xlab("Importance (Mean Decrease Gini)") +
  ylab("Variable") +
  theme(text = element_text(color = "#22211d", size = 14),
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47"),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47"),
        plot.background = element_blank(),
        #panel.background = element_blank(),
        legend.background = element_blank(),
  )+ 
  ggsave("output/rf_importance.png", device = "png", bg = "transparent")
#

# Food deserts, poverty, and transportation ------------------------------------

# Poverty and transportation
ggplot(all_train, aes(x = povertyrate / 100, y = pct_hhold_no_veh)) +
  geom_point(aes(color = fd_2017_label), alpha = 0.7) +
  labs(title = "Census Tract Poverty Rate versus Vehicle Access",
       color = "Food Desert") +
  xlab("Poverty Rate") +
  ylab("% Households with No Vehicle") +
  theme(legend.position = c(0.80, 0.15),
        text = element_text(color = "#22211d", size = 14),
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47"),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
  ) + 
  ggsave("output/poverty_versus_vehicle.png", device = "png", 
         bg = "transparent")


# Need to join variables at the county level.
food_desert_summary_county$county <- str_c(str_trim(
  food_desert_summary_county$county, side = "both"), " County")

food_desert_summary_county <- left_join(food_desert_summary_county, 
                  unique(ruca[c("select_county", "county")]), 
                  by = c("county" = "select_county"))
food_desert_summary_county <- left_join(food_desert_summary_county,
                  pop,
                  by = c("county.y" = "county"))
food_desert_summary_county$mean_poverty_rate_tract <- 
  food_desert_summary_county$mean_poverty_rate_tract / 100

# Plot county poverty rates, food deserts, and %Black
ggplot(food_desert_summary_county, aes(x = mean_poverty_rate_tract,
                                       y = pct_food_desert_tract_2017)) +
  geom_point(aes(color = pct_black_pop_2018, size = pct_black_pop_2018)) +
  labs(title = "County Poverty Rate versus Food Desert Rate",
       subtitle = "Size and Color determined by % County Population (Black)",
       size = "Size: % Black (est)",
       color = "Color: % Black (est)") +
  xlab("Poverty Rate") +
  ylab("% Food Desert Tracts") +
  theme(text = element_text(color = "#22211d", size = 14),
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47"),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
  ) +
  scale_color_gradient2(midpoint = 0.3, low="blue", high="black") + 
  ggsave("output/poverty_versus_fd.png", device = "png", 
         bg = "transparent")


