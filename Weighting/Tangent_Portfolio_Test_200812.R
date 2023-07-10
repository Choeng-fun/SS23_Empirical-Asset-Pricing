
library(data.table)
library(dplyr)
library(openxlsx)

load("~/Desktop/Empirical Asset Pricing/Repo/Data/Stock_List/monthly_3_cum_20.RData")
load("~/Desktop/Empirical Asset Pricing/Repo/Original_Dataset/GBR_DS_monthly.RData")

monthly_3_cum_20 <- as.data.table(monthly_3_cum_20)

Stock_List_200812 <- as.list(monthly_3_cum_20[ym=="2008.12",Id])

Return_Set <- DS.monthly %>% 
  dplyr::select(Id, ym, RET.USD) %>% 
  filter(Id %in% Stock_List_200812) %>% 
  filter(ym > 2006.99999) %>% 
  group_by(Id) %>% 
  slice(1:24) %>% 
  dcast(.,ym ~ Id, value.var = "RET.USD") %>% 
  dplyr::select(-ym)

set(Return_Set, j = names(Return_Set), value = Return_Set/100)



save(Return_Set, file = "/Users/yuruchen/Desktop/Finished Course/21WS and 22SS/Asset Management/练习题excel/Return_Set.csv")
write.xlsx(Return_Set, "/Users/yuruchen/Desktop/Finished Course/21WS and 22SS/Asset Management/练习题excel/Return_Set.xlsx")



excess_return_table <- set(Return_Set, j = names(Return_Set), value = lapply(Return_Set, function(x) x - mean(x)))

excess_return_table_2 <- excess_return_table[1:2]

cov_mat <- cov(excess_return_table)



