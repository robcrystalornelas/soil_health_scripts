## load libraries
library(metafor)
library(metaviz)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/SOC_topsoil_analysis/soil_health_clean_data_SOC_topsoil.R"
)

raw_data_SOC_topsoil <- soil_health_raw_data_topsoil_SOC
head(raw_data_SOC_topsoil)

## First, calculate effect sizes, based on the Ratio of Means (or Response Ratio) for each measurement  in our database
effect_sizes_SOC_topsoil <-
  escalc(
    "ROM", # Specify the effect size we want to calculate
    m1i = raw_data_SOC_topsoil$treatment_mean_standardized_SOC_ten, # provide means for group 1
    n1i = raw_data_SOC_topsoil$treatment_n, # treatment sample size
    sd1i = raw_data_SOC_topsoil$treatment_sd_standardized_SOC_ten, # treatment SD
    m2i = raw_data_SOC_topsoil$control_mean_standardized_SOC_ten, # control mean
    n2i = raw_data_SOC_topsoil$control_n, # control sample size
    sd2i = raw_data_SOC_topsoil$control_sd_standardized_SOC_ten, # control SD
    data = raw_data_SOC_topsoil
  )
head(effect_sizes_SOC_topsoil)

# Run the mixed effects model: article is assigned as a random effect 
mixed_effects_SOC_topsoil <- rma.mv(yi, vi, random = ~ 1 | study_code, data = effect_sizes_SOC_topsoil)
mixed_effects_SOC_topsoil
AIC(mixed_effects_SOC_topsoil)
AIC(mixed_effects_SOC_combined)

# Make forest plot showing SOC results
forest_plot_SOC_topsoil <- viz_forest(
  x = mixed_effects_SOC_topsoil,
  method = "REML",
  xlab = "Response Ratio",
  # make a label along x-axis for effect size
  col = "Reds",
  study_labels = effect_sizes_SOC_topsoil$first_author,
  summary_label = "Summary Effect",
  type = "standard")
forest_plot_SOC_topsoil

# funnel plot - explore this more, do I have to transform these?
viz_sunset(effect_sizes_SOC_topsoil[, c("yi", "vi")], true_effect = .1573, sig_level = .1, power_contours = 'continuous')
