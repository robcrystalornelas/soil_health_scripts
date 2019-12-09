## load libraries
library(metafor)
library(metaviz)

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

# Inspect resulting dataframe
head(effect_sizes_SOC)

# Which farming systems have more than 5 measurements
plyr::count(effect_sizes_SOC$focal_organic_system)

# Filter so that only farming systems that have more than 5 measurements remain
systems_with_more_than_five <- effect_sizes_SOC %>% filter(focal_organic_system == "cover crop" | focal_organic_system == "organic amendment" | focal_organic_system == "organic amendment and till and cover crop" | focal_organic_system == "tillage")
head(systems_with_more_than_five)
dim(systems_with_more_than_five)

## Mixed effect model for farming system
mixed_effect_organic_system <- rma(
  yi,  # outcome
  vi,  # measure of variance
  mods = ~ focal_organic_system - 1, # multiple moderating variables modeled as main effects
  method = "REML",
  data = systems_with_more_than_five,
  weighted = TRUE,
  slab = paste(first_author, publication_year, sep = "")
)
mixed_effect_organic_system

# first, if we want to add in any summary-level info create a new summary table
summary_table_farming_systems <- data.frame(
  "Focal Organic System" = c("Cover Crop","Organic Amendment","OA + Till + CC","Tillage"),
  N = c(11,53,33,21))
head(summary_table_farming_systems)

me_forest_plot_farming_system <-
  viz_forest(
    x = mixed_effect_organic_system,
    method = "REML",
    type = "summary_only",
    summary_table = summary_table_farming_systems,
    confidence_level = 0.95,
    xlab = "Response Ratio",
    col = "Greys",
    text_size = 6,
    annotate_CI = TRUE
  )
me_forest_plot_farming_system

pdf(file = "~/Desktop/research/UMD_org_soil_MA/UMD_project/figures/forest_plot_subgroup_farming_system.pdf", width = 18, height = 5)
me_forest_plot_farming_system
dev.off()
dev.off()
dev.off()

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
