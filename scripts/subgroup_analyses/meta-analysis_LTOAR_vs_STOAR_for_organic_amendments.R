## load libraries
library(metafor)
library(metaviz)
library(cowplot)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/soil_health_clean_data_combined_soil_layers.R"
)

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
head(effect_sizes_SOC_combined)


# Inspect resulting dataframe
head(effect_sizes_SOC_combined)

# Which farming systems have more than 5 measurements
plyr::count(effect_sizes_SOC_combined$focal_organic_system)

# Filter so that only farming systems that have more than 5 measurements remain
organic_amendment_systems <- effect_sizes_SOC_combined %>% filter(focal_organic_system == "organic amendment")

# Mixed effects model for organic amendment STOAR and LTOAR
mixed_effect_LTOAR_vs_STOAR_for_organic_amendments <-
  rma.mv(
    yi,
    vi,
    mods = ~ LTOAR_or_STOAR - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = organic_amendment_systems
  )
mixed_effect_LTOAR_vs_STOAR_for_organic_amendments


##### plotting with metafor
plyr::count(organic_amendment_systems$LTOAR_or_STOAR)
LTOAR_STOAR_samplesize_OA_only <- c(9,35)

forest_plot_LTOAR_vs_STOAR_only_organic_amendments <- forest(mixed_effect_LTOAR_vs_STOAR_for_organic_amendments$b,
                                    ci.lb = mixed_effect_LTOAR_vs_STOAR_for_organic_amendments$ci.lb,
                                    ci.ub = mixed_effect_LTOAR_vs_STOAR_for_organic_amendments$ci.ub,
                                    ilab = LTOAR_STOAR_samplesize_OA_only,
                                    ilab.xpos = c(-.1),
                                    annotate = TRUE,
                                    xlab = "ln(Response Ratio)",
                                    slab = c("LTOAR", "STOAR"),
                                    cex = 1.5,
)

op <- par(cex=1.5, font=2)
text(-.45, 3.1, "Duration")
text(-.1, 3.1, "Sample Size")
text(.85, 3.1, "ln(Response Ratio) [95% CI]")
dev.off()
