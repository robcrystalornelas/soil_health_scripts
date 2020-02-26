## load libraries
library(metafor)
library(metaviz)
library(cowplot)
library(lme4)
library(stargazer)

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


# Filter so that only farming systems that have more than 5 measurements remain
systems_with_more_than_five <- effect_sizes_SOC_topsoil %>% filter(focal_organic_system == "cover crop" | focal_organic_system == "organic amendment" | focal_organic_system == "tillage")
head(systems_with_more_than_five)
dim(systems_with_more_than_five)

# Outcome is `yi` the effect size for each study
# Mixed effects model with random effect assigned to different organic system levels.
management_as_moderator <- lmer(yi ~ (1 | focal_organic_system) + organically_managed_years, data = systems_with_more_than_five)
summary(management_as_moderator)
confint(management_as_moderator)

stargazer(management_as_moderator, type = "html",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "",
          dep.var.labels   = "ln(Response Ratio)",
          out = "~/Desktop/research/UMD_org_soil_MA/figures/model_table.html")

# Years of mgmt figure
system_SOC_and_years_of_mgmt_plot <-
  ggplot(data = systems_with_more_than_five,
         aes(
           x = organically_managed_years,
           y = yi,
           col = as.factor(focal_organic_system)
         )) +
  scale_colour_manual(values = c("#482677FF", "#287D8EFF", "#73D055FF")) +
  geom_point(size = 3,
             alpha = .8,
             position = "jitter") +
  geom_smooth(
    method = lm,
    se = TRUE,
    size = 1,
    alpha = .15,
    fill = "#33638DFF"
  ) +
  theme_cowplot() +
  ggtitle("ln(Response ratio)") +
  xlab("Years under organic management") +
  theme(
    axis.title = element_text(size = 25, face = "bold"),
    title = element_text(size = 25, face = "bold", hjust = -0.5),
    axis.text = element_text(size = 25),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    plot.title = element_text(hjust = -.2, size = 25)
  ) +
  labs(col = "Organic system") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    size = .3
  )
system_SOC_and_years_of_mgmt_plot
