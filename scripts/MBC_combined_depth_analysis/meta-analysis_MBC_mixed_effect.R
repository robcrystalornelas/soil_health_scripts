## load libraries
library(metafor)
library(metaviz)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/MBC_combined_depth_analysis/soil_health_clean_data_MBC_combined.R"
)


raw_data_MBC_combined <- soil_health_raw_data_combined_MBC
head(raw_data_MBC_combined)
## First, calculate effect sizes, based on the Ratio of Means (or Response Ratio) for each measurement  in our database
effect_sizes_MBC <-
  escalc(
    "ROM", # Specify the effect size we want to calculate
    m1i = raw_data_MBC_combined$weighted_treatment_mean_standardized, # provide means for group 1
    n1i = raw_data_MBC_combined$treatment_n, # treatment sample size
    sd1i = raw_data_MBC_combined$weighted_treatment_sd_standardized, # treatment SD
    m2i = raw_data_MBC_combined$weighted_control_mean_standardized, # control mean
    n2i = raw_data_MBC_combined$control_n, # control sample size
    sd2i = raw_data_MBC_combined$weighted_control_sd_standardized, # control SD
    data = raw_data_MBC_combined
  )

# Run the random effects model
random_effects_model_MBC <- rma(
  yi = yi, # specify where effect sizes are located in dataframe
  vi = vi, # specify where variances are located in data frame
  method = "REML", # model fitting
  test = "knha", # Knapp and Hartung is commonly used to test for significance in random effects or mixed effects models
  data = effect_sizes_MBC
)
random_effects_model_MBC

# Run the mixed-effects model
mixed_effects_MBC_combined <- rma.mv(yi, vi, random = ~ 1 | study_code, data = effect_sizes_MBC)
mixed_effects_MBC_combined

# Make forest plot showing SOC results
# forest_plot_MBC_mixed_effects <- viz_forest(
#   x = mixed_effects_MBC_combined,
#   method = "REML",
#   xlab = "ln(Response Ratio)",
#   # make a label along x-axis for effect size
#   col = "Reds",
#   study_labels = effect_sizes_MBC$first_author,
#   summary_label = "Summary Effect",
#   type = "standard",
#   text_size = 7)
# forest_plot_MBC_mixed_effects
# dev.off()

forest(mixed_effects_MBC_combined,
       #ilab = subgroup_samplesize,
       #ilab.xpos = c(-.30),
       annotate = FALSE,
       xlab = "ln(Response Ratio)",
       slab = effect_sizes_MBC$first_author,
       cex = 2,
       col = "#F66B4D",
       mlab = "Summary")

