
library(data.table)
library(tidyr)
library(tidyverse)
library(rollRegres)
library(roll)
library(zoo)
library(dplyr)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

load("~/Desktop/Empirical Asset Pricing/Repo/Univariate_Sort_Set.RData")



# B/Mm contains many NA value and will make the time series discontinuous, so we ignore this variable
# Remove the first 36 months for each stock because Alpha_3 is NA, 
VAR_5_Set <- Univariate_Sort_Set[!is.na(Alpha_3),c("Id","ym","RET.USD","Sigma_3", "CEI", "Log_MV", "Alpha_3", "RSI")]
VAR_3_Set <- Univariate_Sort_Set[!is.na(Alpha_3),c("Id","ym","RET.USD","Log_MV","Alpha_3","RSI")]


# Check if there is missing value and have many stocks have missing
VAR_3_Set[,check_NA_using_sum:=RET.USD+Log_MV+Alpha_3+RSI]
length(unique(VAR_3_Set[is.na(check_NA_using_sum),]$Id))
VAR_3_Set[,check_NA_using_sum:=NULL]
VAR_5_Set[,check_NA_using_sum:=RET.USD+Sigma_3+CEI+Log_MV+Alpha_3+RSI]
length(unique(VAR_5_Set[is.na(check_NA_using_sum),]$Id))
VAR_5_Set[,check_NA_using_sum:=NULL]

# filter out stocks that have a history less than 36 months
Stock_list_long_history <- as.list(VAR_3_Set[,.N, by=Id][N>36]$Id)
VAR_3_Set <- VAR_3_Set[Id %in% Stock_list_long_history]
VAR_5_Set <- VAR_5_Set[Id %in% Stock_list_long_history]
Stock_list <- as.list(unique(VAR_3_Set$Id))

# Replace -Inf in log_MV as the minimum number -4.60517
VAR_3_Set[is.infinite(Log_MV), Log_MV := -4.60517]
VAR_5_Set[is.infinite(Log_MV), Log_MV := -4.60517]


# Function creation
VAR_Func <- function(stocks){

  VAR_3_Set_RETfcst <- data.table(
    Id = character(),
    ym = character(),
    RET.USD = integer(),
    Log_MV = integer(),
    Alpha_3 = integer(),
    RSI = integer(),
    RET_VARfcst = integer()
  )
  n=0

  for (stock in c(Stock_list)){
    print(n)
    n <- n+1
    #print("START")
    # print(stock)
    n_pred_period <- nrow(VAR_3_Set[Id==stock,])-36
    # print(n_pred_period)
    Single_Stock_Set <- VAR_3_Set[Id==stock,]
    # print(Single_Stock_Set)
    Single_Stock_36M_Set <- Single_Stock_Set[1:36]
    # print(Single_Stock_36M_Set)
    Single_Stock_Training_Set <- Single_Stock_36M_Set %>% mutate(Id=NULL) %>% mutate(ym=NULL)
    # print(Single_Stock_Training_Set)
    lag_order <- VARselect(Single_Stock_Training_Set, lag.max = 10, type = "both")$selection[1]
    # print(lag_order)
    VARModel <- VAR(Single_Stock_Training_Set, p = lag_order, type = "const", season = NULL, exog = NULL) 
    # print(VARModel)
    forecast <- predict(VARModel, n.ahead = n_pred_period, ci = 0.95)
    RET_VARfcst <- forecast$fcst$RET.USD
    RET_list <- as.list(RET_VARfcst[1:n_pred_period])
    RET_list <- c(rep(NA,36),RET_list)
    Single_Stock_Set$RET_VARfcst <- RET_list
    VAR_3_Set_RETfcst <- rbind(VAR_3_Set_RETfcst,Single_Stock_Set)
  }
  return(VAR_3_Set_RETfcst)
}
VAR_3_Set_RETfcst <- VAR_Func(Stock_list)



# remove the first 36 month data, where the RETfcst are NA
VAR_3_Set_RETfcst$RET_VARfcst <- as.numeric(VAR_3_Set_RETfcst$RET_VARfcst)
VAR_3_Set_RETfcst_NoNA <- VAR_3_Set_RETfcst[!is.na(RET_VARfcst),]

setorder(VAR_3_Set_RETfcst_NoNA,ym,RET_VARfcst)

top_20 <- VAR_3_Set_RETfcst_NoNA %>%
  group_by(ym) %>%
  slice_max(order_by = RET_VARfcst, n = 20) %>%
  dplyr::select(Id,ym,RET.USD)






