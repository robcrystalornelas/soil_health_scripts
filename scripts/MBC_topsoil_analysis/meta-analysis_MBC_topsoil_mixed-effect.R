## load libraries
library(metafor)
library(metaviz)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/MBC_topsoil_analysis/soil_health_clean_data_MBC_topsoil.R"
)


raw_data_MBC_topsoil <- soil_health_raw_data_topsoil_MBC
raw_data_MBC_topsoil

## First, calculate effect sizes, based on the Ratio of Means (or Response Ratio) for each measurement  in our database
effect_sizes_MBC_topsoil <-
  escalc(
    "ROM", # Specify the effect size we want to calculate
    m1i = raw_data_MBC_topsoil$treatment_mean_MBC_ten, # provide means for group 1
    n1i = raw_data_MBC_topsoil$treatment_n, # treatment sample size
    sd1i = raw_data_MBC_topsoil$treatment_SD_MBC_ten, # treatment SD
    m2i = raw_data_MBC_topsoil$control_mean_MBC_ten, # control mean
    n2i = raw_data_MBC_topsoil$control_n, # control sample size
    sd2i = raw_data_MBC_topsoil$control_SD_MBC_ten, # control SD
    data = raw_data_MBC_topsoil
  )
effect_sizes_MBC_topsoil

# Run the random effects model
random_effects_model_MBC_topsoil <- rma(
  yi = yi, # specify where effect sizes are located in dataframe
  vi = vi, # specify where variances are located in data frame
  method = "REML", # model fitting
  test = "knha", # Knapp and Hartung is commonly used to test for significance in random effects or mixed effects models
  data = effect_sizes_MBC_topsoil
)
random_effects_model_MBC_topsoil

# Run the mixed-effects model
mixed_effects_MBC_topsoil <- rma.mv(yi, vi, random = ~ 1 | study_code, data = effect_sizes_MBC_topsoil)
summary(mixed_effects_MBC_topsoil)
AIC(mixed_effects_MBC_combined)
AIC(mixed_effects_MBC_topsoil)

# Make forest plot showing SOC results
# forest_plot_MBC_mixed_effects_topsoil <- viz_forest(
#   x = mixed_effects_MBC_topsoil,
#   method = "REML",
#   xlab = "ln(Response Ratio)",
#   # make a label along x-axis for effect size
#   col = "Reds",
#   study_labels = effect_sizes_MBC_topsoil$first_author,
#   summary_label = "Summary Effect",
#   type = "standard",
#   text_size = 7)
# forest_plot_MBC_mixed_effects_topsoil
# 
# dev.off()
## Alternative forest plot
forest(mixed_effects_MBC_topsoil,
                          #ilab = subgroup_samplesize,
                          #ilab.xpos = c(-.30),
                          annotate = FALSE,
                          xlab = "ln(Response Ratio)",
                          slab = effect_sizes_MBC_topsoil$first_author,
                          cex = 2,
                          col = "#F66B4D",
                          mlab = "Summary")
dev.off()
