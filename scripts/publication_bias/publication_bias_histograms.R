library(metafor)
library(cowplot)

# Import all data

# MBC combined
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/MBC_combined_depth_analysis/soil_health_clean_data_MBC_combined.R"
)


raw_data_MBC_combined <- soil_health_raw_data_combined_MBC
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


## import data
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


# Here are the four datasets we're using
head(effect_sizes_MBC_combined)
effect_sizes_MBC_combined$yi
head(effect_sizes_MBC_topsoil)
effect_sizes_MBC_topsoil$yi
head(effect_sizes_SOC_combined)
effect_sizes_SOC_topsoil$yi
head(effect_sizes_SOC_topsoil)
effect_sizes_SOC_topsoil$yi

# SOC combined
SOC_combined_hist <-ggplot(effect_sizes_SOC_combined, aes(x=yi)) + 
  geom_histogram(binwidth=0.05,color="black", fill="white") +
  ggtitle("SOC weighted average") +
  xlab("ln(Response ratio)") +
  ylab ("") + 
  theme_cowplot() +
  theme(plot.title = element_text(size = 25, hjust = -.06, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 15)) +
  
  scale_y_continuous(expand = c(0,0),limits=c(0,20))
SOC_combined_hist

# SOC topsoil
SOC_topsoil_hist <-ggplot(effect_sizes_SOC_topsoil, aes(x=yi)) + 
  geom_histogram(binwidth=0.05,color="black", fill="white") +
  ggtitle("SOC frequency") +
  xlab("ln(Response ratio)") +
  ylab ("") + 
  theme_cowplot() +
  theme(plot.title = element_text(size = 25, hjust = -.06, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 15)) +
scale_y_continuous(expand = c(0,0),limits=c(0,20))
SOC_topsoil_hist

#MBC combined
MBC_combined_hist <-ggplot(effect_sizes_MBC_combined, aes(x=yi)) + 
  geom_histogram(binwidth=0.05,color="black", fill="white") +
  ggtitle("MBC weighted average frequency")+
  xlab("ln(Response ratio)") +
  ylab ("") + 
  theme_cowplot() +
  theme(plot.title = element_text(size = 25, hjust = -.06, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 15)) +
  scale_y_continuous(expand = c(0,0),limits=c(0,4))
MBC_combined_hist

#MBC topsoil
MBC_topsoil_hist <-ggplot(effect_sizes_MBC_topsoil, aes(x=yi)) + 
  geom_histogram(binwidth=0.05,color="black", fill="white") +
  ggtitle("MBC frequency")+
  xlab("ln(Response ratio)") +
  ylab ("") + 
  theme_cowplot() +
  theme(plot.title = element_text(size = 25, hjust = -.06, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 15)) +
  scale_y_continuous(expand = c(0,0),limits=c(0,3))
MBC_topsoil_hist

plot_grid(SOC_topsoil_hist,MBC_topsoil_hist, labels = c('a', 'b'), label_size = 25)


