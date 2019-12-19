## load libraries
library(metafor)
library(metaviz)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/soil_health_clean_data_full.R"
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

# Run the mixed effects model: article is assigned as a random effect 
mixed_effects_SOC <- rma.mv(yi, vi, random = ~ 1 | study_code, data = effect_sizes_SOC)
mixed_effects_SOC

# Make forest plot showing SOC results
forest_plot_SOC <- viz_forest(
  x = mixed_effects_SOC,
  method = "REML",
  xlab = "Response Ratio",
  # make a label along x-axis for effect size
  col = "Reds",
  study_labels = effect_sizes_SOC$first_author,
  summary_label = "Summary Effect",
  type = "standard")
forest_plot_SOC

# funnel plot - explore this more, do I have to transform these?
viz_sunset(effect_sizes_SOC[, c("yi", "vi")], true_effect = .14, sig_level = .1, power_contours = 'continuous')
