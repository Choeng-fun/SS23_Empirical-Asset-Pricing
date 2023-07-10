

load("~/Desktop/Empirical Asset Pricing/Repo/Style Analysis/StyleAnalysis_FF_and_Mom_Set.Rdata")

quarterly_3_cum <- read_excel("Style Analysis/quarterly_3_cum.xlsx")

quarterly_3_cum$ym <- paste(substr(quarterly_3_cum$ym,1,4),substr(quarterly_3_cum$ym,6,7), sep=".")

StyleAnalysis_Set <- merge(StyleAnalysis_FF_and_Mom_Set,quarterly_3_cum,by="ym")

Style_Analysis_Model <- lm(RET~SMB+HML+CMA+RMW+WML, data=StyleAnalysis_Set)

summary(Style_Analysis_Model)
