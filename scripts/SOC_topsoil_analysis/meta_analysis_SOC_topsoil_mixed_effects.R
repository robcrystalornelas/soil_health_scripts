## load libraries
library(metafor)
library(metaviz)
library(forestplot)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/SOC_topsoil_analysis/soil_health_clean_data_SOC_topsoil.R"
)

raw_data_SOC_topsoil <- soil_health_raw_data_topsoil_SOC
head(raw_data_SOC_topsoil)
dim(raw_data_SOC_topsoil)
plyr::count(raw_data_SOC_topsoil$focal_organic_system)

## First, calculate effect sizes, based on the Ratio of Means (or Response Ratio) for each measurement  in our database
effect_sizes_SOC_topsoil <-
  escalc(
    "ROM", # Specify the effect size we want to calculate
    m1i = raw_data_SOC_topsoil$treatment_mean_standardized_SOC, # provide means for group 1
    n1i = raw_data_SOC_topsoil$treatment_n, # treatment sample size
    sd1i = raw_data_SOC_topsoil$treatment_sd_standardized_SOC, # treatment SD
    m2i = raw_data_SOC_topsoil$control_mean_standardized_SOC, # control mean
    n2i = raw_data_SOC_topsoil$control_n, # control sample size
    sd2i = raw_data_SOC_topsoil$control_sd_standardized_SOC, # control SD
    data = raw_data_SOC_topsoil
  )
dim(effect_sizes_SOC_topsoil)

# Then, reorder the dataset based on organic farming system
effect_sizes_SOC_topsoil <- effect_sizes_SOC_topsoil[order(effect_sizes_SOC_topsoil$focal_organic_system),]
effect_sizes_SOC_topsoil$focal_organic_system
dim(effect_sizes_SOC_topsoil)

# Run the mixed effects model: article is assigned as a random effect 
mixed_effects_SOC_topsoil <- rma.mv(yi, vi, random = ~ 1 | study_code, data = effect_sizes_SOC_topsoil)
mixed_effects_SOC_topsoil

AIC(mixed_effects_SOC_topsoil)
AIC(mixed_effects_SOC_combined)

## Make forest plot
# First, get labels, so that we don't repeat farming systems
plyr::count(effect_sizes_SOC_topsoil$focal_organic_system)
full_ma_study_label <- c(
  "biochar",
  strrep("", 1:1),
  "cover crop",
  strrep("", 1:8),
  "crop rotation diversity",
  strrep("", 1:1),
  "green manure",
  strrep("", 1:3),
  "organic amendment",
  strrep("", 1:49),
  "organic amendment and till",
  strrep("", 1:3),
  "organic amendment and till and cover crop",
  strrep("", 1:32),
  "tillage",
  strrep("", 1:11))
full_ma_study_label

plyr::count(effect_sizes_SOC_topsoil$focal_organic_system)
par(mar = c(5.1, 4.1, .8, 2.1)) # first number is bottom, 2nd is left, third is top margins
forest(
  effect_sizes_SOC_topsoil$yi,
  effect_sizes_SOC_topsoil$vi,
  annotate = FALSE,
  xlab = "ln(Response Ratio)",
  slab = full_ma_study_label,
  ylim = c(-1,120),
  cex = 1.3,
  pch = 15,
  cex.lab = 1.5,
  col = c(
    rep('#73D055FF', 2),
    rep('#cc6a70ff', 9),
    rep("#1F968BFF", 2),
    rep('#F66B4D', 4),
    rep('#481567FF', 50),
    rep('#f9b641ff', 4),
    rep('#404788FF', 33),
    rep ("#3CBB75FF", 12)))

addpoly(mixed_effects_SOC_topsoil, cex = 1.3, col ="black", annotate = TRUE, mlab = "Summary")
dev.off()

