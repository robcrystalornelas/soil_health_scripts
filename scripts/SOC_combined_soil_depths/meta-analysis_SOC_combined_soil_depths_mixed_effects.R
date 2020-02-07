## load libraries
library(metafor)
library(metaviz)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/SOC_combined_soil_depths/soil_health_clean_data_SOC_combined.R"
)

raw_data_SOC_combined <- soil_health_raw_data_combined_SOC 
raw_data_SOC_combined

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

# Run the mixed effects model: article is assigned as a random effect 
mixed_effects_SOC_combined <- rma.mv(yi, vi, random = ~ 1 | study_code, data = effect_sizes_SOC_combined)
mixed_effects_SOC_combined

# Make forest plot showing SOC results
# forest_plot_SOC_combined <- viz_forest(
#   x = mixed_effects_SOC_combined,
#   method = "REML",
#   xlab = "ln(Response Ratio)",
#   # make a label along x-axis for effect size
#   col = "Reds",
#   study_labels = effect_sizes_SOC_combined$first_author,
#   summary_label = "Summary Effect",
#   type = "standard")
# forest_plot_SOC_combined

study_numbers <- c(1:116)
forest(mixed_effects_SOC_combined,
       annotate = FALSE,
       xlab = "ln(Response Ratio)",
       slab = study_numbers,
       cex = .8,
       col = "#F66B4D",
       mlab = "Summary")