library(readr)
library(dplyr)
library(ggplot2)
theme_set(theme_minimal())

library(readr)
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



merge.data <- merge(X3_cum_cumulative_return, X3_var_cumulative_return, all=TRUE)
merge.data <- merge(merge.data, X3_mean_cumulative_return, all=TRUE)
merge.data <- merge(merge.data, X5_var_cumulative_return, all=TRUE)
merge.data <- merge(merge.data, X6_mean_cumulative_return, all=TRUE)
merge.data <- merge(merge.data, X6_cum_cumulative_return, all=TRUE)

plot.data <- merge.data %>%
  select(date, X3_cum_cumulative_return, X3_var_cumulative_return, X5_var_cumulative_return, X6_mean_cumulative_return, 
         X6_cum_cumulative_return, X3_mean_cumulative_return) %>%
  gather(key = "variable", value = "value", -date)

ggplot(plot.data, aes(x = date, y = value)) + 
  geom_line(aes(color = variable))
# + scale_color_manual(values = c("purple", "darkgreen", "darkred", "steelblue"))