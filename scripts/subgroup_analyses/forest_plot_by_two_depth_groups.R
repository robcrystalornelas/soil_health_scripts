## load libraries
library(metafor)
library(metaviz)
library(cowplot)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/SOC_topsoil_analysis/soil_health_clean_data_SOC_topsoil.R"
)

raw_data_SOC_topsoil <- soil_health_raw_data_topsoil_SOC

## First, calculate effect sizes, based on the Ratio of Means (or Response Ratio) for each measurement  in our database
effect_sizes_SOC_topsoil <-
  escalc(
    "ROM", # Specify the effect size we want to calculate
    m1i = raw_data_SOC_topsoil$treatment_mean_standardized_SOC, # provide means for group 1
    n1i = raw_data_SOC_topsoil$treatment_n, # treatment sample size
    sd1i = raw_data_SOC_topsoil$treatment_sd_standardized_SOC, # treatment SD
    m2i = raw_data_SOC_topsoil$control_mean_standardized_SOC, # control mean
    n2i = raw_data_SOC_topsoil$control_n, # control sample size
    sd2i = raw_data_SOC_topsoil$control_sd_standardized_SOC, # control SD
    data = raw_data_SOC_topsoil
  )
dim(effect_sizes_SOC_topsoil)

# Which farming systems have more than 5 measurements
plyr::count(effect_sizes_SOC_topsoil$focal_organic_system)

# Filter so that only farming systems that have more than 5 measurements remain
organic_amendment_effect_sizes <- effect_sizes_SOC_topsoil %>% filter(focal_organic_system == "organic amendment")
no_till_effect_sizes <- effect_sizes_SOC_topsoil %>% filter(focal_organic_system == "tillage")
cover_crop_effect_sizes <- effect_sizes_SOC_topsoil %>% filter(focal_organic_system == "cover crop")

# Drop NAs before fitting model
organic_amendment_effect_sizes <- droplevels(organic_amendment_effect_sizes[!organic_amendment_effect_sizes$first_author == 'Singh',])
organic_amendment_effect_sizes

## Mixed effects model for organic amendments by depth
mixed_effect_oa_two_depths <-
  rma.mv(
    yi,
    vi,
    mods = ~ depth_measured_binned_two_groups - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = organic_amendment_effect_sizes
  )
mixed_effect_oa_two_depths

# Determine subgroup sample sizes
plyr::count(organic_amendment_effect_sizes$depth_measured_binned_two_groups)
oa_subgroup_sample_size <- c(25,11)

forest_plot_oa_two_group_depth <- forest(mixed_effect_oa_two_depths$b,
                                     ci.lb = mixed_effect_oa_two_depths$ci.lb,
                                     ci.ub = mixed_effect_oa_two_depths$ci.ub,
                                     ilab = oa_subgroup_sample_size,
                                     ilab.xpos = c(-.3),
                                     annotate = TRUE,
                                     xlab = "ln(Response Ratio)",
                                     alim = c(-0.2, 0.4),
                                     xlim = c(-0.6,0.8),
                                     slab = c("0-15cm","0-50cm"),
                                     cex = 2,
                                     digits = 2)

op <- par(cex=2, font=2)
text(-.55, 3.1, "Depth")
text(-.3, 3.1, "Sample Size")
text(.58, 3.1, "ln(Response Ratio) [95% CI]")
dev.off()

# Second forest plot with no till

mixed_effect_notill_two_group_depth <-
  rma.mv(
    yi,
    vi,
    mods = ~ depth_measured_binned_two_groups - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = no_till_effect_sizes
  )
mixed_effect_notill_two_group_depth

# Determine subgroup sample sizes
plyr::count(no_till_effect_sizes$depth_measured_binned_two_groups)
no_till_subgroup_sample_size <- c(6,6)
forest_plot_notill_two_group_depth <- forest(mixed_effect_notill_two_group_depth$b,
                                                 ci.lb = mixed_effect_notill_two_group_depth$ci.lb,
                                                 ci.ub = mixed_effect_notill_two_group_depth$ci.ub,
                                                 ilab = no_till_subgroup_sample_size,
                                                 ilab.xpos = c(-.3),
                                                 annotate = TRUE,
                                                 xlab = "ln(Response Ratio)",
                                             alim = c(-0.2, 0.4),
                                             xlim = c(-0.6,0.8),
                                                  slab = c("0-15cm", "0-50cm"),
                                                 cex = 2,
                                                 digits = 2)

op <- par(cex=2, font=2)
text(-.55, 3.1, "Depth")
text(-.3, 3.1, "Sample Size")
text(.58, 3.1, "ln(Response Ratio) [95% CI]")
dev.off()

# Third forest plot with cover crops
mixed_effect_cc_two_group_depth <-
  rma.mv(
    yi,
    vi,
    mods = ~ depth_measured_binned_two_groups - 1,
    random = ~ 1 | study_code,
    method = "REML",
    digits = 4,
    data = cover_crop_effect_sizes
  )
mixed_effect_cc_two_group_depth

# Determine subgroup sample sizes
plyr::count(cover_crop_effect_sizes$depth_measured_binned_two_groups)
cc_subgroup_sample_size <- c(3,5)

forest_plot_cc_two_depths <- forest(mixed_effect_cc_two_group_depth$b,
                                       ci.lb = mixed_effect_cc_two_group_depth$ci.lb,
                                       ci.ub = mixed_effect_cc_two_group_depth$ci.ub,
                                       ilab = cc_subgroup_sample_size,
                                       ilab.xpos = c(-.3),
                                       annotate = TRUE,
                                       xlab = "ln(Response Ratio)",
                                       slab = c("0-15cm","0-50cm"),
                                    alim = c(-0.2, 0.4),
                                    xlim = c(-0.6,0.8),
                                       cex = 2,
                                       digits = 2
)

op <- par(cex=2, font=2)
text(-.55, 3.1, "Depth")
text(-.3, 3.1, "Sample Size")
text(.58, 3.1, "ln(Response Ratio) [95% CI]")
dev.off()
