## load data ####
source("~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/soil_health_clean_data.R")

## load libraries ####
library(ggplot2)
library(cowplot)

## Make figur
gg <-
  ggplot(soil_health_raw_data, aes(
    x = reorder(outcome_being_measured, outcome_being_measured, function(x)
      -
        length(x))
  ))

gg <- gg + geom_bar(stat = "count", fill = "#3CBB75FF") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 70))
gg

gg <- gg + theme_cowplot()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("")
gg <-
  gg + theme(
    axis.text = element_text(size = 23),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 23
    ),
    strip.text = element_text(size = 23),
    axis.title = element_text(size = 23)
  ) # Change axis title size
gg
