
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

Stock_list_3 <- Stock_list[3]

Stock_list_10 <- Stock_list[480:490]

# Replace -Inf in log_MV as the minimum number -4.60517
VAR_3_Set[is.infinite(Log_MV), Log_MV := -4.60517]
VAR_5_Set[is.infinite(Log_MV), Log_MV := -4.60517]

# Replace -Inf in CEI as the minimum number -4.60517
VAR_5_Set[is.infinite(CEI)&CEI>0, CEI := 8.537883]
VAR_5_Set[is.infinite(CEI)&CEI<0, CEI := -33.5675]


# Replace Inf in CEI as the minimum number -4.60517

# 3 factor cumulative var
VAR_3_Func <- function(stocks){
  
  VAR_3_Set_RETfcst <- data.table(
    Id = character(),
    ym = character(),
    RET.USD = integer(),
    Log_MV = integer(),
    Alpha_3 = integer(),
    RSI = integer(),
    RET_VARfcst = integer()
  )
  j=0
  
  for (stock in c(Stock_list)){
    n=0
    n_pred_period <- nrow(VAR_3_Set[Id==stock,])-36
    Single_Stock_Set <- VAR_3_Set[Id==stock,]
    RET_list <- rep(0,36)

    for (n in 1:n_pred_period){
      Single_Stock_training_Set <- Single_Stock_Set[1:36+n]
      Single_Stock_Training_Set <- Single_Stock_training_Set %>% mutate(Id=NULL) %>% mutate(ym=NULL)
      lag_order <- VARselect(Single_Stock_Training_Set, lag.max = 10, type = "both")$selection[1]
      VARModel <- VAR(Single_Stock_Training_Set, p = lag_order, type = "const", season = NULL, exog = NULL) 
      forecast <- predict(VARModel, n.ahead = 1, ci = 0.95)
      RET_VARfcst <- forecast$fcst$RET.USD
      RET_OnePeriod <- as.numeric(RET_VARfcst[1])
      RET_list <- c(RET_list,RET_OnePeriod)
    }
    Single_Stock_Set$RET_VARfcst <- RET_list
    VAR_3_Set_RETfcst <- rbind(VAR_3_Set_RETfcst,Single_Stock_Set)
    print(j)
    j <- j+1
  }
  
  return(VAR_3_Set_RETfcst)
}

VAR_3_Set_RETfcst <- VAR_3_Func(Stock_list)

VAR_3_Set_RETfcst_copy <- VAR_3_Set_RETfcst

monthly_3_var_20 <- VAR_3_Set_RETfcst %>% 
  group_by(Id) %>% 
  slice(-(1:36)) %>% 
  ungroup() %>% 
  dplyr::select(Id, ym, RET.USD, RET_VARfcst) %>% 
  group_by(ym) %>% 
  slice_max(order_by = RET_VARfcst, n=20) %>% 
  dplyr::select(Id, ym)


quarterly_3_var_20 <- monthly_3_var_20

monthly_3_var_10 <- VAR_3_Set_RETfcst %>% 
  group_by(Id) %>% 
  slice(-(1:36)) %>% 
  ungroup() %>% 
  dplyr::select(Id, ym, RET.USD, RET_VARfcst) %>% 
  group_by(ym) %>% 
  top_frac(0.1, wt = RET_VARfcst) %>% 
  dplyr::select(Id, ym)


quarterly_3_var_10 <- monthly_3_var_10



save(monthly_3_var_20,file="/Users/yuruchen/Desktop/Empirical Asset Pricing/Repo/Data/Stock_List/monthly_3_var_20.RData")
save(quarterly_3_var_20,file="/Users/yuruchen/Desktop/Empirical Asset Pricing/Repo/Data//Stock_List/quarterly_3_var_20.RData")
save(monthly_3_var_10,file="/Users/yuruchen/Desktop/Empirical Asset Pricing/Repo/Data/Stock_List/monthly_3_var_10.RData")
save(quarterly_3_var_10,file="/Users/yuruchen/Desktop/Empirical Asset Pricing/Repo/Data//Stock_List/quarterly_3_var_10.RData")


# 5 factor cumulative var
VAR_5_Func <- function(stocks){
  
  VAR_5_Set_RETfcst <- data.table(
    Id = character(),
    ym = character(),
    RET.USD = integer(),
    Sigma_3 = integer(),
    CEI = integer(),
    Log_MV = integer(),
    Alpha_3 = integer(),
    RSI = integer(),
    RET_VARfcst = integer()
  )
  j=0
  
  for (stock in c(stocks)){
    n=0
    n_pred_period <- nrow(VAR_5_Set[Id==stock,])-36
    Single_Stock_Set <- VAR_5_Set[Id==stock,]
    RET_list <- rep(NA,36)
    
    for (n in 1:n_pred_period){
      
      Single_Stock_training_Set <- Single_Stock_Set[1:36+n]
      Single_Stock_Training_Set <- Single_Stock_training_Set %>% mutate(Id=NULL) %>% mutate(ym=NULL)
      lag_order <- VARselect(Single_Stock_Training_Set, lag.max = 3, type = "both")$selection[1]
      VARModel <- VAR(Single_Stock_Training_Set, p = 2, type = "const", season = NULL, exog = NULL) 
      forecast <- predict(VARModel, n.ahead = 1, ci = 0.95)
      RET_VARfcst <- forecast$fcst$RET.USD
      RET_OnePeriod <- as.numeric(RET_VARfcst[1])
      RET_list <- c(RET_list,RET_OnePeriod)
    }
    Single_Stock_Set$RET_VARfcst <- RET_list
    VAR_5_Set_RETfcst <- rbind(VAR_5_Set_RETfcst,Single_Stock_Set)
    print(j)
    j <- j+1
  }
  
  return(VAR_5_Set_RETfcst)
}

VAR_5_Set_RETfcst <- VAR_5_Func(Stock_list)

VAR_5_Set_RETfcst_copy <- VAR_5_Set_RETfcst


monthly_5_var_20 <- VAR_5_Set_RETfcst %>% 
  group_by(Id) %>% 
  slice(-(1:36)) %>% 
  ungroup() %>% 
  dplyr::select(Id, ym, RET.USD, RET_VARfcst) %>% 
  group_by(ym) %>% 
  slice_max(order_by = RET_VARfcst, n=20) %>% 
  dplyr::select(Id, ym)


quarterly_5_var_20 <- monthly_5_var_20

monthly_5_var_10 <- VAR_5_Set_RETfcst %>% 
  group_by(Id) %>% 
  slice(-(1:36)) %>% 
  ungroup() %>% 
  dplyr::select(Id, ym, RET.USD, RET_VARfcst) %>% 
  group_by(ym) %>% 
  top_frac(0.1, wt = RET_VARfcst) %>% 
  dplyr::select(Id, ym)


quarterly_5_var_10 <- monthly_5_var_10



save(monthly_5_var_20,file="/Users/yuruchen/Desktop/Empirical Asset Pricing/Repo/Data/Stock_List/monthly_5_var_20.RData")
save(quarterly_5_var_20,file="/Users/yuruchen/Desktop/Empirical Asset Pricing/Repo/Data//Stock_List/quarterly_5_var_20.RData")
save(monthly_5_var_10,file="/Users/yuruchen/Desktop/Empirical Asset Pricing/Repo/Data/Stock_List/monthly_5_var_10.RData")
save(quarterly_5_var_10,file="/Users/yuruchen/Desktop/Empirical Asset Pricing/Repo/Data//Stock_List/quarterly_5_var_10.RData")





















# 
# 
# # 3 factor rolling var
# VAR_3_Func_Rolling <- function(stocks){
#   
#   VAR_3_Set_RETfcst_Rolling <- data.table(
#     Id = character(),
#     ym = character(),
#     RET.USD = integer(),
#     Log_MV = integer(),
#     Alpha_3 = integer(),
#     RSI = integer(),
#     RET_VARfcst = integer()
#   )
#   j=0
#   
#   for (stock in c(stocks)){
#     n=0
#     n_pred_period <- nrow(VAR_3_Set[Id==stock,])-36
#     Single_Stock_Set <- VAR_3_Set[Id==stock,]
#     RET_list <- rep(NA,36)
#     
#     for (n in 1:n_pred_period){
#       Single_Stock_training_Set <- Single_Stock_Set[1+n:36+n]
#       print("A")
#       Single_Stock_Training_Set <- Single_Stock_training_Set %>% mutate(Id=NULL) %>% mutate(ym=NULL)
#       print("B")
#       #lag_order <- VARselect(Single_Stock_Training_Set, lag.max = 1, type = "both")$selection[1]
#       print("C")
#       VARModel <- VAR(Single_Stock_Training_Set, p = 1, type = "const", season = NULL, exog = NULL) 
#       print("D")
#       forecast <- predict(VARModel, n.ahead = 1, ci = 0.95)
#       print("E")
#       RET_VARfcst <- forecast$fcst$RET.USD
#       print("F")
#       RET_OnePeriod <- as.numeric(RET_VARfcst[1])
#       print("G")
#       RET_list <- c(RET_list,RET_OnePeriod)
#       print(n)
#     }
#     Single_Stock_Set$RET_VARfcst <- RET_list
#     VAR_3_Set_RETfcst_Rolling <- rbind(VAR_3_Set_RETfcst_Rolling,Single_Stock_Set)
#     print(j)
#     j <- j+1
#   }
#   
#   return(VAR_3_Set_RETfcst_Rolling)
# }
# 
# VAR_3_Set_RETfcst_Rolling <- VAR_3_Func_Rolling(Stock_list_3)
# 
# 
# VAR_3_Set_RETfcst_Rolling_copy <- VAR_3_Set_RETfcst_Rolling
# 
