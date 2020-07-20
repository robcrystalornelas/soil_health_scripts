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
    m1i = raw_data_MBC_topsoil$treatment_mean, # provide means for group 1
    n1i = raw_data_MBC_topsoil$treatment_n, # treatment sample size
    sd1i = raw_data_MBC_topsoil$treatment_sd, # treatment SD
    m2i = raw_data_MBC_topsoil$control_mean, # control mean
    n2i = raw_data_MBC_topsoil$control_n, # control sample size
    sd2i = raw_data_MBC_topsoil$control_sd, # control SD
    data = raw_data_MBC_topsoil
  )
effect_sizes_MBC_topsoil

# Then, reorder the dataset based on organic farming system
effect_sizes_MBC_topsoil <- effect_sizes_MBC_topsoil[order(effect_sizes_MBC_topsoil$focal_organic_system),]
effect_sizes_MBC_topsoil$focal_organic_system


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

# Create forest plot ####
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

## Make forest plot
# First, get labels, so that we don't repeat farming systems
plyr::count(effect_sizes_MBC_topsoil$focal_organic_system)
full_ma_study_label_MBC <- c(
  "green manure",
  "organic amendment",
  strrep("", 1:9),
  "tillage",
  strrep("", 1:1))
full_ma_study_label_MBC

plyr::count(effect_sizes_MBC_topsoil$focal_organic_system)
par(mar = c(5.1, 4.1, .8, 2.1)) # first number is bottom, 2nd is left, third is top margins
forest(
  effect_sizes_MBC_topsoil$yi,
  effect_sizes_MBC_topsoil$vi,
  annotate = FALSE,
  xlab = "ln(Response Ratio)",
  slab = full_ma_study_label_MBC,
  ylim = c(0,16),
  cex = 1.5,
  pch = 15,
  col = c(
    rep('#482677FF', 1),
    rep('#287D8EFF', 10),
    rep('#73D055FF', 2)))
addpoly(mixed_effects_MBC_topsoil, row = 0 , cex = 1.5,col ="black", annotate = TRUE, mlab = "Summary")
dev.off()
