## load data ####

soil_health_raw_data_full <- read.csv("~/Desktop/research/UMD_org_soil_MA/Crystal-Ornelas_Thapa_Tully_datasheet - datasheet_v2.csv", header = TRUE)

## load libraries ####
library(ggplot2)
library(cowplot)
library(maps)

# Create map of world
head(soil_health_raw_data_full)
world_map <- map_data("world")
map_of_study_sites<- ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="#F7F7F7", colour = "black", lwd = .25) +
  geom_point(data = soil_health_raw_data_full, aes(x=longitude, y=latitude) ,color="red", inherit.aes = FALSE) +
  theme_cowplot() +
  coord_equal(ratio=1)
map_of_study_sites

unique_country_and_code<- select(soil_health_raw_data_full, study_code, study_location)
unique_country_and_code <- unique(unique_country_and_code)
plyr::count(unique_country_and_code$study_location)
