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

# Then, reorder the dataset based on organic farming system
effect_sizes_SOC_combined <- effect_sizes_SOC_combined[order(effect_sizes_SOC_combined$focal_organic_system),]
effect_sizes_SOC_combined$focal_organic_system

# Run the mixed effects model: article is assigned as a random effect 
mixed_effects_SOC_combined <- rma.mv(yi, vi, random = ~ 1 | study_code, data = effect_sizes_SOC_combined)
mixed_effects_SOC_combined
## Make forest plot
# First, get labels, so that we don't repeat farming systems
plyr::count(effect_sizes_SOC_combined$focal_organic_system)
full_ma_study_label <- c(
  "biochar",
  strrep("", 1:1),
  "cover crop",
  strrep("", 1:7),
  "cover crop and organic amendment",
  "crop rotation",
  strrep("", 1:1),
  "green manure",
  strrep("", 1:2),
  "green manure and organic amendment",
  "organic amendment",
  strrep("", 1:49),
  "organic amendment and till",
  strrep("", 1:3),
  "organic amendment and till and cover crop",
  strrep("", 1:32),
  "tillage",
  strrep("", 1:11))

plyr::count(effect_sizes_SOC_combined$focal_organic_system)
par(mar = c(5.1, 4.1, .8, 2.1)) # first number is bottom, 2nd is left, third is top margins
forest(
  effect_sizes_SOC_combined$yi,
  effect_sizes_SOC_combined$vi,
  annotate = FALSE,
  xlab = "ln(Response Ratio)",
  slab = full_ma_study_label,
  ylim = c(-1,120),
  cex = 1.3,
  pch = 15,
  col = c(
    rep('#481567FF', 2),
    rep('#cc6a70ff', 8),
    rep('#DCE319FF', 1),
    rep("#F66B4D", 2),
    rep('#1F968BFF', 3),
    rep("#81176D", 1),
    rep('#73D055FF', 50),
    rep('#3CBB75FF', 4),
    rep('#404788FF', 33),
    rep ("#f9b641ff", 12)))

addpoly(mixed_effects_SOC_combined, cex = 1.3, col ="black", annotate = TRUE, mlab = "Summary")
dev.off()
