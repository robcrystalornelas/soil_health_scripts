library(metafor)
library(cowplot)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/MBC_combined_depth_analysis/soil_health_clean_data_MBC_combined.R"
)


raw_data_MBC_combined <- soil_health_raw_data_combined_MBC
head(raw_data_MBC_combined)
## First, calculate effect sizes, based on the Ratio of Means (or Response Ratio) for each measurement  in our database
effect_sizes_MBC_combined <-
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

# Import SOC
## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/SOC_combined_soil_depths/soil_health_clean_data_SOC_combined.R"
)

raw_data_SOC_combined <- soil_health_raw_data_combined_SOC
dim(raw_data_SOC_combined)

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

## Now run SOC meta-analysis without outlier
## For SOC, outlier is the singh paper
SOC_without_outlier <- droplevels(effect_sizes_SOC_combined[!effect_sizes_SOC_combined$first_author == 'Singh',])
mixed_effects_SOC_no_outlier <- rma.mv(yi, vi, random = ~ 1 | study_code, data = SOC_without_outlier)
summary(mixed_effects_SOC_no_outlier)

study_numbers <- c(1:102)


par(mfrow=c(1,2))

forest(mixed_effects_SOC_no_outlier,
       annotate = FALSE,
       xlab = "ln(Response Ratio)",
       slab = study_numbers,
       cex = .8,
       col = "#F66B4D",
       mlab = "Summary",
       ilab.xpos= - 1.5)

## Now run MBC meta-analysis without outlier
## Removing Jindo study
effect_sizes_MBC_combined
MBC_without_outlier <- droplevels(effect_sizes_MBC_combined[!effect_sizes_MBC_combined$first_author == 'Jindo',])
MBC_without_outlier <- droplevels(MBC_without_outlier[!MBC_without_outlier$first_author == 'Sun',])
levels(MBC_without_outlier$first_author)

mixed_effects_MBC_no_outlier <- rma.mv(yi, vi, random = ~ 1 | study_code, data = MBC_without_outlier)
summary(mixed_effects_MBC_no_outlier)

forest(mixed_effects_MBC_no_outlier,
       annotate = FALSE,
       xlab = "ln(Response Ratio)",
       slab = MBC_without_outlier$first_author,
       cex = 2,
       col = "#F66B4D",
       mlab = "Summary",
       ilab.xpos= - 1.5)


