## load libraries
library(metafor)
library(metaviz)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/soil_health_clean_data_combined_soil_layers.R"
)

## First, calculate effect sizes, based on the Ratio of Means (or Response Ratio) for each measurement  in our database
effect_sizes_SOC_combined <-
  escalc(
    "ROM", # Specify the effect size we want to calculate
    m1i = raw_data_SOC_combined$weighted_treatment_mean_standardized, # provide means for group 1
    n1i = raw_data_SOC_combined$treatment_n, # treatment sample size
    sd1i = raw_data_SOC_combined$weighted_treatment_sd_standardized, # treatment SD
    m2i = raw_data_SOC_combined$weighted_control_mean_standardized, # control mean
    n2i = raw_data_SOC_combined$control_n, # control sample size
    sd2i = raw_data_SOC_combined$weighted_control_sd_standardized, # control SD
    data = raw_data_SOC_combined
  )

# Inspect resulting dataframe
head(effect_sizes_SOC_combined)

# Which farming systems have more than 5 measurements
plyr::count(effect_sizes_SOC_combined$focal_organic_system)

# Filter so that only farming systems that have more than 5 measurements remain
systems_with_more_than_five <- effect_sizes_SOC_combined %>% filter(focal_organic_system == "cover crop" | focal_organic_system == "organic amendment" | focal_organic_system == "tillage")
head(systems_with_more_than_five)
dim(systems_with_more_than_five)

## Mixed effect model for farming system
# mixed_effect_organic_system <- rma(
#   yi,  # outcome
#   vi,  # measure of variance
#   mods = ~ focal_organic_system - 1, # multiple moderating variables modeled as main effects
#   method = "REML",
#   data = systems_with_more_than_five,
#   weighted = TRUE,
#   slab = paste(first_author, publication_year, sep = "")
# )
# mixed_effect_organic_system

mixed_effect_organic_system_rmamv <-
  rma.mv(
    yi,
    vi,
    mods = ~ focal_organic_system - 1,
    random = ~ 1 | study_code,
    method = "DL",
    digits = 4,
    data = systems_with_more_than_five
  )
class(mixed_effect_organic_system_rmamv)

forest(mixed_effect_organic_system_rmamv,
       addfit = TRUE,
       addcred = TRUE,
       col = "red",
       addpoly())

# first, if we want to add in any summary-level info create a new summary table
summary_table_farming_systems <- data.frame(
  "System" = c("Cover Crop","Organic Amendment","Tillage"),
  N = c(11,45,10))
head(summary_table_farming_systems)

me_forest_plot_farming_system <-
  viz_forest(
    x = mixed_effect_organic_system_rmamv,
    method = "DL",
    type = "summary_only",
    summary_table = summary_table_farming_systems,
    confidence_level = 0.95,
    xlab = "Response Ratio",
    col = "Blues",
    variant = "rain",
    text = 8,
    annotate_CI = TRUE,
    table_layout = matrix(c(1,2,3), nrow = 1)
)
me_forest_plot_farming_system

### To get all the real data, have to run separate rmas for each nesting type
rma_cover_crop <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_SOC,
  subset = (focal_organic_system=="cover crop"))
rma_cover_crop

rma_oa <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_SOC,
  subset = (focal_organic_system=="organic amendment"))
rma_oa

rma_oa_tillage_cc <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_SOC,
  subset = (focal_organic_system=="organic amendment and till and cover crop"))
rma_oa_tillage_cc

rma_tillage <- rma(
  yi,  # outcome
  vi,  # measure of variance
  method = "REML",
  data = effect_sizes_SOC,
  subset = (focal_organic_system=="tillage"))
rma_tillage
