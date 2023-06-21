library(data.table)
library(tidyr)
library(tidyverse)

load("~/Desktop/Summer Semester/Advanced Seminar/GBR/panel_country_May28.RData")
panel_country <- as.data.table(panel_country)



#Profitability
# Select only necessary variables from the comprehensive data set
panel_reduced_prof<- panel_country[!is.na(RET.USD),c("Id","ym","MV.USD","RET.USD","WC08301")]

# Find the length of history of each stock, and filter out stocks that has a history less than 12 months
count_table <- panel_reduced_prof[,.N,by=Id]
panel_reduced_prof <- panel_reduced_prof[count_table[N>12]$Id]

#WC08301 Return on Equity – Total (%)
panel_reduced_prof <- panel_reduced_prof[,ROE:=WC08301]
panel_reduced_prof <- panel_reduced_prof %>% 
  select(Id, ym, MV.USD, RET.USD, ROE)

# Order stocks based on the past performance, divide them into 10 deciles, and filter the 1st and 10th decile.
panel_reduced_prof <- panel_reduced_prof[!is.na(ROE),c("Id","ym","MV.USD","RET.USD","ROE")]
setorder(panel_reduced_prof,ym,ROE) 
panel_reduced_prof <- panel_reduced_prof[, Decile := ntile(ROE, 10), by = ym]
panel_reduced_prof <- panel_reduced_prof[!is.na(MV.USD) & !is.na(RET.USD) & Decile %in% c(1,10)]

# Long the high profitability (stocks in the 10th decile), short the low profitability(stocks in the 1st decile), and get the momentum return
portfolio_returns_prof <- panel_reduced_prof %>% 
  group_by(ym,Decile)%>% # do "everything" for the groups specified here
  summarize(ret.port = mean(RET.USD)) %>% # calculate mean return for each group
  spread(Decile,ret.port) %>% # create one column for each group
  mutate(Ret_prof = `10`-`1`)

# Test if the momentum return is significantly different from 0
portfolio_returns_prof <- as.data.table(portfolio_returns_prof)
portfolio_returns_prof[,t.test(Ret_prof)]




#Investment
#WC02999 Total Assets
# Select only necessary variables from the comprehensive data set
panel_reduced_inv<- panel_country[!is.na(RET.USD),c("Id","ym","MV.USD","RET.USD","WC02999")]

# Find the length of history of each stock, and filter out stocks that has a history less than 12 months
count_table <- panel_reduced_inv[,.N,by=Id]
panel_reduced_inv <- panel_reduced_inv[count_table[N>12]$Id]

#WC02999 Total Assets
panel_reduced_inv <- panel_reduced_inv %>% 
  mutate(Lagged_Asset = shift(WC02999, n = 12)) %>% 
  mutate(Change_Asset = (WC02999 - Lagged_Asset) / Lagged_Asset *100) %>% 
  select(-Lagged_Asset)

panel_reduced_inv <- panel_reduced_inv %>% 
  select(Id, ym, MV.USD, RET.USD, Change_Asset)

# Order stocks based on the past performance, divide them into 10 deciles, and filter the 1st and 10th decile.
panel_reduced_inv <- panel_reduced_inv[!is.na(Change_Asset),c("Id","ym","MV.USD","RET.USD","Change_Asset")]
setorder(panel_reduced_inv,ym,Change_Asset) 
panel_reduced_inv <- panel_reduced_inv[, Decile := ntile(Change_Asset, 10), by = ym]
panel_reduced_inv <- panel_reduced_inv[!is.na(MV.USD) & !is.na(RET.USD) & Decile %in% c(1,10)]

# Long the conservative (stocks in the 1st decile), short the aggresive(stocks in the 10th decile), and get the momentum return
portfolio_returns_inv <- panel_reduced_inv %>% 
  group_by(ym,Decile)%>% # do "everything" for the groups specified here
  summarize(ret.port = mean(RET.USD)) %>% # calculate mean return for each group
  spread(Decile,ret.port) %>% # create one column for each group
  mutate(Ret_inv = `1`-`10`)

# Test if the momentum return is significantly different from 0
portfolio_returns_inv <- as.data.table(portfolio_returns_inv)
portfolio_returns_inv[,t.test(Ret_inv)]




#Value
# Select only necessary variables from the comprehensive data set
panel_reduced_val<- panel_country[!is.na(RET.USD),c("Id","ym","MV.USD","RET.USD","WC05476", "WC05001")]

# Find the length of history of each stock, and filter out stocks that has a history less than 12 months
count_table <- panel_reduced_prof[,.N,by=Id]
panel_reduced_val <- panel_reduced_val[count_table[N>12]$Id]

#WC05001 Market Price, WC05476 Book Value per Share
panel_reduced_val <- panel_reduced_val[,BTM := WC05476/WC05001]
panel_reduced_val <- panel_reduced_val %>% 
  select(Id, ym, MV.USD, RET.USD, BTM)

# Order stocks based on the past performance, divide them into 10 deciles, and filter the 1st and 10th decile.
panel_reduced_val <- panel_reduced_val[!is.na(BTM),c("Id","ym","MV.USD","RET.USD","BTM")]
setorder(panel_reduced_val,ym,BTM) 
panel_reduced_val <- panel_reduced_val[, Decile := ntile(BTM, 10), by = ym]
panel_reduced_val <- panel_reduced_val[!is.na(MV.USD) & !is.na(RET.USD) & Decile %in% c(1,10)]

# Long the high profitability (stocks in the 10th decile), short the low profitability(stocks in the 1st decile), and get the momentum return
portfolio_returns_val <- panel_reduced_val %>% 
  group_by(ym,Decile)%>% # do "everything" for the groups specified here
  summarize(ret.port = mean(RET.USD)) %>% # calculate mean return for each group
  spread(Decile,ret.port) %>% # create one column for each group
  mutate(Ret_val = `10`-`1`)

# Test if the momentum return is significantly different from 0
portfolio_returns_val <- as.data.table(portfolio_returns_val)
portfolio_returns_val[,t.test(Ret_val)]



#Momentum
# Select only necessary variables from the comprehensive data set
panel_reduced_rsi <- panel_country[!is.na(RET.USD),c("Id","ym","MV.USD","RET.USD")]

# Find the length of history of each stock, and filter out stocks that has a history less than 12 months
count_table <- panel_reduced_rsi[,.N,by=Id]
panel_reduced_rsi <- panel_reduced_rsi[count_table[N>12]$Id]


panel_reduced_rsi[, gain := ifelse(RET.USD>0, RET.USD, 0)]
panel_reduced_rsi[, loss := ifelse(RET.USD<0, -RET.USD, 0)]

panel_reduced_rsi[, average_gain := frollmean(gain, 12, align = "right"), by = Id]
panel_reduced_rsi[, average_loss := frollmean(loss, 12, align = "right"), by = Id]
panel_reduced_rsi[, RS := average_gain/average_loss]
panel_reduced_rsi[, RSI := 100 - (100/(1+RS))]


# Order stocks based on the past performance, divide them into 10 deciles, and filter the 1st and 10th decile.
panel_reduced_rsi <- panel_reduced_rsi[!is.na(RSI),c("Id","ym","MV.USD","RET.USD","RSI")]
setorder(panel_reduced_rsi,ym,RSI) 
panel_reduced_rsi <- panel_reduced_rsi[, Decile := ntile(RSI, 10), by = ym]
panel_reduced_rsi <- panel_reduced_rsi[!is.na(MV.USD) & !is.na(RET.USD) & Decile %in% c(1,10)]

# Long the past winners (stocks in the 10th decile), short the past losers(stocks in the 1st decile), and get the momentum return
portfolio_returns_rsi <- panel_reduced_rsi %>% 
  group_by(ym,Decile)%>% # do "everything" for the groups specified here
  summarize(ret.port = mean(RET.USD)) %>% # calculate mean return for each group
  spread(Decile,ret.port) %>% # create one column for each group
  mutate(Ret_rsi = `10`-`1`)

# Test if the momentum return is significantly different from 0
portfolio_returns_rsi <- as.data.table(portfolio_returns_rsi)
portfolio_returns_rsi[,t.test(Ret_rsi)]


