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

# Inspect resulting dataframe
head(effect_sizes_SOC_combined)

# Which farming systems have more than 5 measurements
plyr::count(effect_sizes_SOC_combined$focal_organic_system)

# Filter so that only farming systems that have more than 5 measurements remain
systems_with_more_than_five <- effect_sizes_SOC_combined %>% filter(focal_organic_system == "cover crop" | focal_organic_system == "organic amendment" | focal_organic_system == "tillage")
head(systems_with_more_than_five)
dim(systems_with_more_than_five)

mixed_effect_organic_system_rmamv <-
  rma.mv(
    yi,
    vi,
    mods = ~ focal_organic_system - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = systems_with_more_than_five
  )
mixed_effect_organic_system_rmamv

subgroup_samplesize <- c(8,49,12)
forest_plot_farming_system <- forest(mixed_effect_organic_system_rmamv$b,
       ci.lb = mixed_effect_organic_system_rmamv$ci.lb,
       ci.ub = mixed_effect_organic_system_rmamv$ci.ub,
       ilab = subgroup_samplesize,
       ilab.xpos = c(-.20),
       annotate = TRUE,
       xlab = "ln(Response Ratio)",
       slab = c("Cover Crop", "Organic Amendment","Tillage"),
       cex = 2,
       )

op <- par(cex=2, font=2)
text(-.38, 4.2, "Organic Farming System")
text(-.15, 4.2, "Sample Size")
text(.63, 4.2, "ln(Response Ratio) [95% CI]")
dev.off()
