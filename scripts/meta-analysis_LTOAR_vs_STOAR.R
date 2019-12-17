## load libraries
library(metafor)
library(metaviz)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/soil_health_clean_data.R"
)

## First, calculate effect sizes, based on the Ratio of Means (or Response Ratio) for each measurement  in our database
effect_sizes_SOC <-
  escalc(
    "ROM", # Specify the effect size we want to calculate
    m1i = raw_data_SOC$treatment_mean_standardized, # provide means for group 1
    n1i = raw_data_SOC$treatment_n, # treatment sample size
    sd1i = raw_data_SOC$treatment_sd_standardized, # treatment SD
    m2i = raw_data_SOC$control_mean_standardized, # control mean
    n2i = raw_data_SOC$control_n, # control sample size
    sd2i = raw_data_SOC$control_sd_standardized, # control SD
    data = raw_data_SOC
  )

# Inspect resulting dataframe
head(effect_sizes_SOC)

mixed_effect_LTOAR_vs_STOAR <-
  rma.mv(
    yi,
    vi,
    mods = ~ LTOAR_or_STOAR - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = effect_sizes_SOC
  )
mixed_effect_LTOAR_vs_STOAR

# Prepare forest plot labels
plyr::count(effect_sizes_SOC$LTOAR_or_STOAR)
summary_table_LTOAR <- data.frame(
  "Duration" = c("LTOAR","STOAR"),
  N = c(33,96))

# Forest plot
me_forest_plot_LTOAR_vs_STOAR <-
  viz_forest(
    x = mixed_effect_LTOAR_vs_STOAR,
    method = "REML",
    type = "summary_only",
    summary_table = summary_table_LTOAR,
    confidence_level = 0.95,
    xlab = "Response Ratio",
    col = "Greens",
    variant = "rain",
    x_limit = c(-.025,.45),
    annotate_CI = TRUE
  )
me_forest_plot_LTOAR_vs_STOAR

