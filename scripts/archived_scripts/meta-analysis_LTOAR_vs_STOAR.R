## load libraries
library(metafor)
library(metaviz)

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

mixed_effect_LTOAR_vs_STOAR <-
  rma.mv(
    yi,
    vi,
    mods = ~ LTOAR_or_STOAR - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = effect_sizes_SOC_combined
  )
mixed_effect_LTOAR_vs_STOAR

##### plotting with metafor
plyr::count(effect_sizes_SOC_combined$LTOAR_or_STOAR)
LTOAR_STOAR_samplesize <- c(19,85)


forest_plot_LTOAR_vs_STOAR<- forest(mixed_effect_LTOAR_vs_STOAR$b,
                                     ci.lb = mixed_effect_LTOAR_vs_STOAR$ci.lb,
                                     ci.ub = mixed_effect_LTOAR_vs_STOAR$ci.ub,
                                     ilab = LTOAR_STOAR_samplesize,
                                     ilab.xpos = c(-.1),
                                     annotate = TRUE,
                                     xlab = "ln(Response Ratio)",
                                     slab = c("LTOAR", "STOAR"),
                                     cex = 1.5,
)

op <- par(cex=1.5, font=2)
text(-.3, 3.1, "Duration")
text(-.1, 3.1, "Sample Size")
text(.58, 3.1, "ln(Response Ratio) [95% CI]")
