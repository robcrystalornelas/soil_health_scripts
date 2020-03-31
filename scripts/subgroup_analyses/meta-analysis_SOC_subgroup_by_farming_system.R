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

# Which farming systems have more than 5 measurements
plyr::count(effect_sizes_SOC_topsoil$focal_organic_system)

# Filter so that only farming systems that have more than 5 measurements remain
systems_with_more_than_five <- effect_sizes_SOC_topsoil %>% filter(focal_organic_system == "cover crop" | focal_organic_system == "organic amendment" | focal_organic_system == "tillage")
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

subgroup_samplesize <- c(7,50,12)
forest_plot_farming_system <- forest(mixed_effect_organic_system_rmamv$b,
       ci.lb = mixed_effect_organic_system_rmamv$ci.lb,
       ci.ub = mixed_effect_organic_system_rmamv$ci.ub,
       ilab = subgroup_samplesize,
       ilab.xpos = c(-.2),
       annotate = TRUE,
       xlab = "ln(Response Ratio)",
       slab = c("Cover Crop", "Organic Amendment","Tillage"),
       cex = 2,
       )

op <- par(cex=2, font=2)
text(-.43, 4.2, "Organic Farming Practice")
text(-.17, 4.2, "Sample Size")
text(.65, 4.2, "ln(Response Ratio) [95% CI]")
dev.off()
