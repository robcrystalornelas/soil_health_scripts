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
# forest_plot_SOC_topsoil <- viz_forest(
#   x = mixed_effects_SOC_topsoil,
#   method = "REML",
#   xlab = "ln(Response Ratio)",
#   # make a label along x-axis for effect size
#   col = "Reds",
#   study_labels = effect_sizes_SOC_topsoil$first_author,
#   summary_label = "Summary Effect",
#   type = "standard")
# forest_plot_SOC_topsoil

## alternative forest plot
#
study_numbers <- c(1:116)
forest(mixed_effects_SOC_topsoil,
       annotate = FALSE,
       xlab = "ln(Response Ratio)",
       slab = study_numbers,
       cex = .8,
       col = "#F66B4D",
       mlab = "Summary")

# This shows vector of ordering by focal organic system
order(effect_sizes_SOC_topsoil$focal_organic_system)
# Forest plot grouped
study_numbers <- c(1:116)
plyr::count(effect_sizes_SOC_topsoil$focal_organic_system)

full_ma_study_label <- c(
  "biochar",
  strrep("", 1:1),
  "cover crop",
  strrep("", 1:6),
  "crop rotation diversity",
  strrep("", 1:1),
  "green manure",
  strrep("", 1:3),
  "green manure and organic amendment",
  strrep("", 1:1),
  "organic amendment",
  strrep("", 1:49),
  "organic amendment and till",
  strrep("", 1:3),
  "organic amendment and till and cover crop",
  strrep("", 1:32),
  "tillage",
  strrep("", 1:11))
full_ma_study_label

forest(mixed_effects_SOC_topsoil,
       annotate = FALSE,
       xlab = "ln(Response Ratio)",
       slab = full_ma_study_label,
       cex = .9,
       col = "#F66B4D",
       mlab = "Summary",
       order = order(effect_sizes_SOC_topsoil$focal_organic_system))
