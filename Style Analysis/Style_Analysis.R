

library(readxl)
library(dplyr)

load(
  "~/Desktop/Empirical Asset Pricing/Repo/Style Analysis/StyleAnalysis_FF_and_Mom_Set.Rdata"
)
load("~/Desktop/Empirical Asset Pricing/repo/DTB6_monthly_July01.RData")
load(
  "~/Desktop/Empirical Asset Pricing/Repo/Performance/visual_annual&cumulative_result/mean_6_performance.RData"
)

Market_RET <- read_excel("Market_RET.xlsx")

RMRF_df <- merge(DTB6_monthly, Market_RET, by = "ym")
RMRF_df <-
  RMRF_df %>% mutate(RMRF = Market_RET - DTB6) %>% select(ym, RMRF)

StyleAnalysis_FF_and_Mom_Set <-
  merge(StyleAnalysis_FF_and_Mom_Set, RMRF_df, by = "ym")


quarterly_6_mean_performance$ym <-
  paste(
    substr(quarterly_6_mean_performance$ym, 1, 4),
    substr(quarterly_6_mean_performance$ym, 6, 7),
    sep = "."
  )

StyleAnalysis_Set <-
  merge(StyleAnalysis_FF_and_Mom_Set,
        quarterly_6_mean_performance,
        by = "ym")

Style_Analysis_Model <-
  lm(RET ~ RMRF + SMB + HML + CMA + RMW + WML, data = StyleAnalysis_Set)

summary(Style_Analysis_Model)
