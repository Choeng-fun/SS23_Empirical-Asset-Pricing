library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
theme_set(theme_minimal())
library(readr)
library(data.table)
library(tidyr)

X3_cum_cumulative_return <- read_csv("Performance/visual_annual&cumulative_result/3_cum_cumulative_return.csv", 
                                     col_types = cols(...1 = col_date(format = "%Y-%m-%d")))
X3_cum_cumulative_return <- X3_cum_cumulative_return %>%
  rename(X3_cum_cumulative_return=RET, date=`...1`)

X3_var_cumulative_return <- read_csv("Performance/visual_annual&cumulative_result/3_var_cumulative_return.csv", 
                                     col_types = cols(...1 = col_date(format = "%Y-%m-%d")))
X3_var_cumulative_return <- X3_var_cumulative_return %>%
  rename(X3_var_cumulative_return=RET, date=`...1`)

X3_mean_cumulative_return <- read_csv("Performance/visual_annual&cumulative_result/3_mean_cumulative_return.csv", 
                                      col_types = cols(...1 = col_date(format = "%Y-%m-%d")))
X3_mean_cumulative_return <- X3_mean_cumulative_return %>%
  rename(X3_mean_cumulative_return=RET, date=`...1`)

X5_var_cumulative_return <- read_csv("Performance/visual_annual&cumulative_result/5_var_cumulative_return.csv", 
                                     col_types = cols(...1 = col_date(format = "%Y-%m-%d")))
X5_var_cumulative_return <- X5_var_cumulative_return %>%
  rename(X5_var_cumulative_return=RET, date=`...1`)

X6_mean_cumulative_return <- read_csv("Performance/visual_annual&cumulative_result/6_mean_cumulative_return.csv", 
                                      col_types = cols(...1 = col_date(format = "%Y-%m-%d")))
X6_mean_cumulative_return <- X6_mean_cumulative_return %>%
  rename(X6_mean_cumulative_return=RET, date=`...1`)

X6_cum_cumulative_return <- read_csv("Performance/visual_annual&cumulative_result/6_cum_cumulative_return.csv", 
                                      col_types = cols(...1 = col_date(format = "%Y-%m-%d")))
X6_cum_cumulative_return <- X6_cum_cumulative_return %>%
  rename(X6_cum_cumulative_return=RET, date=`...1`)

load("Univariate_Sort_Set.RData")
Univariate_Sort_Set <- as.data.table(Univariate_Sort_Set)
stock_size <- Univariate_Sort_Set
setorder(stock_size, ym, -MV.USD)
stock_big <- stock_size[!is.na(MV.USD), .(big = ifelse(((cumsum(MV.USD)/sum(MV.USD))>=0.9), 0, 1), Id), by=ym]
stock_big <- stock_big[big==1, c("Id", "ym")]
Univariate_Sort_Set_big <- merge(stock_big, Univariate_Sort_Set)


Market_RET <- Univariate_Sort_Set_big %>% 
  select(Id, ym, RET.USD, MV.USD) %>% 
  group_by(ym) %>% 
  mutate(MRET = weighted.mean(RET.USD, coalesce(MV.USD, 0), na.rm = T)) %>% 
  ungroup %>% 
  arrange(ym, Id) %>% 
  filter(ym >= "1990-01-01")

Market_RET_ym <- Market_RET %>% 
  group_by(ym) %>% 
  summarise(Market_RET = mean(MRET)) %>%
  mutate(date=as.Date(as.yearmon(ym, "%Y.%m"))) %>%
  arrange(date) %>%
  mutate(Benchmark=cumsum(Market_RET)) %>%
  select(date, Benchmark)

merge.data <- merge(X3_cum_cumulative_return, X3_var_cumulative_return, all=TRUE)
merge.data <- merge(merge.data, X3_mean_cumulative_return, all=TRUE)
merge.data <- merge(merge.data, X5_var_cumulative_return, all=TRUE)
merge.data <- merge(merge.data, X6_mean_cumulative_return, all=TRUE)
merge.data <- merge(merge.data, X6_cum_cumulative_return, all=TRUE)
merge.data <- merge(merge.data, X6_cum_cumulative_return, all=TRUE)
merge.data <- merge(merge.data, Market_RET_ym, all=TRUE)

plot.data <- merge.data %>%
  select(date, X3_cum_cumulative_return, X3_var_cumulative_return, X5_var_cumulative_return, X6_mean_cumulative_return, 
         X6_cum_cumulative_return, X3_mean_cumulative_return, Benchmark) %>%
  gather(key = "variable", value = "value", -date)

ggplot(plot.data, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size=0.9) +
  theme(
    # panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #
  ) 
# + scale_color_manual(values = c("purple", "darkgreen", "darkred", "steelblue"))