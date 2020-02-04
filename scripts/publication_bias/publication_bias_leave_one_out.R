library(metafor)
library(cowplot)

# Import MBC
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/MBC_topsoil_analysis/soil_health_clean_data_MBC_topsoil.R"
)


raw_data_MBC_topsoil <- soil_health_raw_data_topsoil_MBC

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

# Import SOC
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

## Now run SOC meta-analysis without outlier
## For SOC, outlier is the singh paper
SOC_without_outlier <- droplevels(effect_sizes_SOC_topsoil[!effect_sizes_SOC_topsoil$first_author == 'Singh',])
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
effect_sizes_MBC_topsoil
MBC_without_outlier <- droplevels(effect_sizes_MBC_topsoil[!effect_sizes_MBC_topsoil$first_author == 'Jindo',])
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


