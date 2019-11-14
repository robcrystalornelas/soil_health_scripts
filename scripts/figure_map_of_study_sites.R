## load data ####
source("~/Desktop/UMD_org_soil_MA/UMD_project/scripts/soil_health_clean_data.R")

## load libraries ####
library(ggplot2)
library(cowplot)

# Create map of world
head(soil_health_raw_data)
world_map <- map_data("world")
map_of_study_sites<- ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="#F7F7F7", colour = "black", lwd = .25) +
  geom_point(data = soil_health_raw_data, aes(x=longitude, y=latitude) ,color="red", inherit.aes = FALSE) +
  theme_cowplot() +
  coord_equal(ratio=1)
map_of_study_sites

