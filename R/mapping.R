# mapping ---------------------------------------------------------------------
# Build maps by Census tract.
# Adapted from https://www.r-graph-gallery.com/168-load-a-shape-file-into-r.html
# If having trouble with packages, restart R
# NC Census tract data from US Census 2017
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
               color = "#f2f2f2", size = 0.3) +
  theme_void()+
  labs(title = "USDA Food Desert Designations 2017", 
       subtitle = "By 2017 US Census Tract",
       fill = "USDA Food Desert 2017") + 
  scale_color_hue(labels = c("Not Food Desert", "Food Desert")) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47"),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47"),
    legend.position = c(0.1, 0.15)
  ) +
  coord_map()


#
