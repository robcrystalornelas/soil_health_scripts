## load libraries
library(metafor)
library(metaviz)
library(cowplot)

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

# Which farming systems have more than 5 measurements
plyr::count(effect_sizes_SOC_combined$focal_organic_system)

# Filter so that only farming systems that have more than 5 measurements remain
organic_amendment_effect_sizes <- effect_sizes_SOC_combined %>% filter(focal_organic_system == "organic amendment")
no_till_effect_sizes <- effect_sizes_SOC_combined %>% filter(focal_organic_system == "tillage")
cover_crop_effect_sizes <- effect_sizes_SOC_combined %>% filter(focal_organic_system == "cover crop")

# Drop NAs before fitting model
organic_amendment_effect_sizes <- droplevels(organic_amendment_effect_sizes[!organic_amendment_effect_sizes$first_author == 'Singh',])

# Mixed effects model for organic amendments by depth
mixed_effect_oa_depth_rmamv <-
  rma.mv(
    yi,
    vi,
    mods = ~ depth_measured_binned - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = organic_amendment_effect_sizes
  )
mixed_effect_oa_depth_rmamv

# Determine subgroup sample sizes
plyr::count(organic_amendment_effect_sizes$depth_measured_binned)
oa_subgroup_sample_size <- c(21,13,2)

forest_plot_organic_amendment_by_depth <- forest(mixed_effect_oa_depth_rmamv$b,
                                     ci.lb = mixed_effect_oa_depth_rmamv$ci.lb,
                                     ci.ub = mixed_effect_oa_depth_rmamv$ci.ub,
                                     # ilab = oa_subgroup_sample_size,
                                     #ilab.xpos = c(-.7),
                                     annotate = TRUE,
                                     xlab = "ln(Response Ratio)",
                                     slab = c("0-15cm", "0-30cm","0-60cm"),
                                     cex = 2,
                                     digits = 2,
                                     col = "#482677FF"
)

text(-.7, rev(seq(3:1)), oa_subgroup_sample_size, col = "#482677FF", cex = 2)
op <- par(cex=2, font=2)
text(-1.25, 4.2, "Depth")
text(-.6, 4.2, "Sample Size")
text(.9, 4.2, "ln(Response Ratio) [95% CI]")
dev.off()

# Second forest plot with no till

mixed_effect_notill_depth_rmamv <-
  rma.mv(
    yi,
    vi,
    mods = ~ depth_measured_binned - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = no_till_effect_sizes
  )
mixed_effect_notill_depth_rmamv

# Determine subgroup sample sizes
plyr::count(no_till_effect_sizes$depth_measured_binned)
no_till_subgroup_sample_size <- c(3,4,5)

forest_plot_no_till_by_depth <- forest(mixed_effect_notill_depth_rmamv$b,
                                                 ci.lb = mixed_effect_notill_depth_rmamv$ci.lb,
                                                 ci.ub = mixed_effect_notill_depth_rmamv$ci.ub,
                                                 #ilab = no_till_subgroup_sample_size,
                                                 #ilab.xpos = c(-.7),
                                                 annotate = TRUE,
                                                 xlab = "ln(Response Ratio)",
                                                 slab = c("0-15cm", "0-30cm","0-60cm"),
                xlim = c(-.5,.5),
                                                 cex = 2,
                                                 digits = 2,
                                       col = "#287D8EFF"
)

text(-.25, rev(seq(3:1)), no_till_subgroup_sample_size, col = "#287D8EFF", cex = 2)
op <- par(cex=2, font=2)
text(-.5, 4.2, "Depth")
text(-.27, 4.2, "Sample Size")
text(.7, 4.2, "ln(Response Ratio) [95% CI]")
dev.off()

# Third forest plot with cover crops
cover_crop_effect_sizes$depth_measured

mixed_effect_cc_depth_rmamv <-
  rma.mv(
    yi,
    vi,
    mods = ~ depth_measured_binned - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = cover_crop_effect_sizes
  )
mixed_effect_cc_depth_rmamv

# Determine subgroup sample sizes
plyr::count(cover_crop_effect_sizes$depth_measured_binned)
cc_subgroup_sample_size <- c(3,2,3)

forest_plot_cc_by_depth <- forest(mixed_effect_cc_depth_rmamv$b,
                                       ci.lb = mixed_effect_cc_depth_rmamv$ci.lb,
                                       ci.ub = mixed_effect_cc_depth_rmamv$ci.ub,
                                       #ilab = no_till_subgroup_sample_size,
                                       #ilab.xpos = c(-.7),
                                       annotate = TRUE,
                                       xlab = "ln(Response Ratio)",
                                       slab = c("0-15cm", "0-30cm","0-60cm"),
                                       cex = 2,
                                       digits = 2,
                                       col = "#73D055FF"
)

text(-.6, rev(seq(3:1)), cc_subgroup_sample_size, col = "#73D055FF", cex = 2)
op <- par(cex=2, font=2)
text(-.97, 4.2, "Depth")
text(-.6, 4.2, "Sample Size")
text(.68, 4.2, "ln(Response Ratio) [95% CI]")
dev.off()

?forest
