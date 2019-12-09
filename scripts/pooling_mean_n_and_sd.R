## load libraries
library(metafor)
library(dmetar)
library(metaviz)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/soil_health_clean_data.R"
)

# Mixed-effects meta-analytic model accounting for multiple measurements coming from single article

pool.groups(n1=50,
            n2=50,
            m1=3.5,
            m2=4,
            sd1=3,
            sd2=3.8)