## load libraries
library(metafor)
library(metaviz)
library(cowplot)
## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/soil_health_clean_data.R"
)

## First, calculate effect sizes, based on the Ratio of Means (or Response Ratio) for each measurement  in our database
effect_sizes_SOC <-
  escalc(
    "ROM", # Specify the effect size we want to calculate
    m1i = raw_data_SOC$treatment_mean_standardized, # provide means for group 1
    n1i = raw_data_SOC$treatment_n, # treatment sample size
    sd1i = raw_data_SOC$treatment_sd_standardized, # treatment SD
    m2i = raw_data_SOC$control_mean_standardized, # control mean
    n2i = raw_data_SOC$control_n, # control sample size
    sd2i = raw_data_SOC$control_sd_standardized, # control SD
    data = raw_data_SOC
  )
head(effect_sizes_SOC)

# Filter so that only farming systems that have more than 5 measurements remain
systems_with_more_than_five <- effect_sizes_SOC %>% filter(focal_organic_system == "cover crop" | focal_organic_system == "organic amendment" | focal_organic_system == "organic amendment and till and cover crop" | focal_organic_system == "tillage")
head(systems_with_more_than_five)
dim(systems_with_more_than_five)

# Figure for trophic position
system_SOC_and_years_of_mgmt_plot <-
  ggplot(data = systems_with_more_than_five,
         aes(
           x = organically_managed_years,
           y = yi,
           col = as.factor(focal_organic_system)
         )) +
  viridis::scale_color_viridis(discrete = TRUE) +
  geom_point(size = 1,
             alpha = .8,
             position = "jitter") +
  geom_smooth(
    method = lm,
    se = FALSE,
    size = 1,
    alpha = .8
  ) +
  theme_cowplot() +
  ylab("ln(Response ratio)") +
  xlab("Organically managed years") +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  ) +
  labs(col = "Organic system") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    size = .3
  )
system_SOC_and_years_of_mgmt_plot


