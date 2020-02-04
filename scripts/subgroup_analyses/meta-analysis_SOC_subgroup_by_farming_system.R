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
       ilab.xpos = c(-.30),
       annotate = TRUE,
       xlab = "ln(Response Ratio)",
       slab = c("Cover Crop", "Organic Amendment","Tillage"),
       cex = 2,
       )

op <- par(cex=2, font=2)
text(-.58, 4.2, "Organic Farming System")
text(-.25, 4.2, "Sample Size")
text(.71, 4.2, "ln(Response Ratio) [95% CI]")
dev.off()


### Now, for systems with more than 3 measurements
systems_with_more_than_three <- effect_sizes_SOC_combined %>% filter(focal_organic_system == "cover crop" | focal_organic_system == "organic amendment" | focal_organic_system == "tillage" | focal_organic_system == "green manure")
head(systems_with_more_than_three)
dim(systems_with_more_than_three)

mixed_effect_organic_system_rmamv_three <-
  rma.mv(
    yi,
    vi,
    mods = ~ focal_organic_system - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = systems_with_more_than_three
  )
mixed_effect_organic_system_rmamv_three

subgroup_samplesize_three_cases <- c(8,4,49,12)
forest_plot_farming_system <- forest(mixed_effect_organic_system_rmamv_three$b,
                                     ci.lb = mixed_effect_organic_system_rmamv_three$ci.lb,
                                     ci.ub = mixed_effect_organic_system_rmamv_three$ci.ub,
                                     ilab = subgroup_samplesize_three_cases,
                                     ilab.xpos = c(-.20),
                                     annotate = TRUE,
                                     xlab = "ln(Response Ratio)",
                                     slab = c("Cover Crop", "Green Manure","Organic Amendment","Tillage"),
                                     cex = 2,
)

op <- par(cex=2, font=2)
text(-.37, 5.2, "Organic Farming System")
text(-.15, 5.2, "Sample Size")
text(.58, 5.2, "ln(Response Ratio) [95% CI]")
dev.off()


