library(data.table)
library(dplyr)
library(lubridate)
library(zoo)

load("Data_Excursus4a.RData")

# RET.USD: return in USD
# LMV.USD: lagged market cap in USD (last month)
# MV.USD.June: we have it because we consruct the portfolio at this time
# BM: book to market ratio (calculated in June): the book value of last year is know in June.

## Determine portfolio breakpoints for Size
# Fama-French take the MV from end-of-June and rebalance yearly
Data_Excursus4a[,month := month(Date)]
Data_Excursus4a[,year := year(Date)]
# hcjun: help column: Value is rebalanced once a year at the end of June. 
Data_Excursus4a[,hcjun := ifelse(month>=7,year,year-1)]

# visualize the size distribution
hist(Data_Excursus4a[month==7 & year==2011,MV.USD.June],breaks=500)
# better use a log scale
hist(Data_Excursus4a[month==7 & year==2011,log(MV.USD.June)],breaks=500)

# determine size portfolio allocation from July on using data that's public from end-of-June on
setorder(Data_Excursus4a,Date,-LMV.USD) 
hlpvariable <-  Data_Excursus4a[month==7 & !is.na(LMV.USD),
                                .(pf.size = ifelse((cumsum(LMV.USD)/sum(LMV.USD))>=0.9,"Small","Big"),Id),
                                by=year]
# 从上往下排，前面89个股票加起来占了90%多，是big股，后面200多个股票加起来占了10%，都是小股

# Merge the size portfolio allocation back from July Y to June Y+1
panel_country <- merge(Data_Excursus4a,hlpvariable,
                       by.x=c("hcjun","Id"),
                       by.y=c("year","Id"),
                       all.x=T)

# Determine the B/M breakpoints based on big stocks only
hlpvariable2 <- panel_country[month==7 & !is.na(BM) & pf.size=="Big", .(bm_bb30 = quantile(BM , probs = c(0.3), na.rm=T),
                                                                        bm_bb70 = quantile(BM , probs = c(0.7), na.rm=T)),by=year]
# 前30%是value stock，后30%是value stock，中间是neutral


# Merge the B/M portfolio allocation back from July Y to June Y+1
panel_country <- merge(panel_country,hlpvariable2,
                       by.x=c("hcjun"),
                       by.y=c("year"),
                       all.x=T)

panel_country[ , pf.bm := ifelse(BM>bm_bb70,"High",ifelse((BM<=bm_bb70 & BM>bm_bb30),"Neutral",ifelse(BM<=bm_bb30,"Low",NA)))]

panel_country[, SIZE_VALUE := paste0(pf.size,".",pf.bm)] 
# for double sort

# fastest way to calculate portfolio returns...
# ...however, try other functions to get more familiar with R
library("tidyverse")

portfolio_returns <- panel_country[!is.na(pf.size) & !is.na(pf.bm)] %>% # this operator nests functions
  group_by(Date,SIZE_VALUE) %>% # do "everything" for the groups specified here
  summarize(ret.port = weighted.mean(RET.USD,
                                     LMV.USD)) %>% # vw returns using lagged mcap
  spread(SIZE_VALUE,ret.port) %>% # create one column for each group
  mutate(
    Small = (Small.High + Small.Neutral + Small.Low)/3, # just exemplary
    Big = (Big.High + Big.Neutral + Big.Low)/3,
    SMB = Small-Big,
    High = (Small.High + Big.High)/2,
    Low = (Small.Low + Big.Low)/2,
    HML = High-Low
  )

portfolio_returns <- as.data.table(portfolio_returns)

# Check the factors
portfolio_returns[,t.test(SMB)]
# mean is neg, it means in Germany small stocks have lower return then big stocks, but not significantly meaningful
portfolio_returns[,t.test(HML)]
# mean is significant pos, 
