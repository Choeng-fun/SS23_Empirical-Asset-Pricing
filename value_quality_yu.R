library(data.table)
library(dplyr)
library(lubridate)
library(zoo)
setwd("C:/Users/cryst/Desktop/SS2023/Empirical Asset Pricing/")
#value1 :Forward price to earnings (Fwd P/E)

# 05001 Market Price
# 05202 Earnings per Share - Fiscal Year End


# ?


#value2 :Enterprise value/operating cash flows (EV/CFO)
# 18100 Enterprise Value
# 04860 Net Cash Flow - Operating Activities
if('WC18100' %in% colnames(panel_country)){
  print('Yes')
}
sum(is.na(panel_country$WC18100))
print('Yes')
if('WC04860' %in% colnames(panel_country)){
  print('Yes')
}
sum(is.na(panel_country$WC04860))


CS.reg.estimtates.contr.1 <- Data_Excursus1[, .(gamma_zero=lm(RET.USD~BETA+LMV.USD)$coefficient[1],
                                                gamma=lm(RET.USD~BETA+LMV.USD)$coefficient[2],
                                                gamma.LMV=lm(RET.USD~BETA+LMV.USD)$coefficient[3],
                                                no.obs=length(Id)),by=ym]
#create a new column EV.CFO
panel_country$EV.CFO <- panel_country$WC18100/panel_country$WC04860
panel_country[20,panel_country$EV.CFO]
sum(is.na(panel_country$EV.CFO))
sum(is.na(panel_country$RET.USD))

newtable_value2 <- panel_country[,.(EV.CFO, RET.USD, ym, Id)]
newtable_value2 <- newtable_value2[!is.na(EV.CFO) & !is.na(RET.USD) & !is.infinite(EV.CFO) & !is.infinite(RET.USD)]

reg.value2 <- newtable_value2[, .(intercept = lm(RET.USD~EV.CFO)$coefficient[1],
                                beta = lm(RET.USD~EV.CFO)$coefficient[2],
                                no.obs = length(Id)),
                            by = ym]
sum(is.infinite(newtable_value2$EV.CFO))
sum(is.na(newtable_value2$RET.USD))

reg.value2[,t.test(intercept)]
reg.value2[,t.test(beta)]


#value3 :Price/Book Value Ratio
# 05001 Market Price
# 05476 Book Value per Share

#PBM <- Common EquityWC05001/Market CapitalizationWC05476
panel_country$PBM <- panel_country$WC05001/panel_country$WC05476

if('PBM' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}

newtable_value3 <- panel_country[,.(PBM, RET.USD, ym, Id)]
newtable_value3 <- newtable_value3[!is.na(PBM) & !is.na(RET.USD) & !is.infinite(PBM) & !is.infinite(RET.USD)]

reg.value3 <- newtable_value3[, .(intercept = lm(RET.USD~PBM)$coefficient[1],
                                  beta = lm(RET.USD~PBM)$coefficient[2],
                                  no.obs = length(Id)),
                              by = ym]

reg.value3[,t.test(intercept)]
reg.value3[,t.test(beta)]



if('WC05476' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}
reg.value2 <- panel_country[, .(intercept = lm(RET.USD~WC09302)$coefficient[1],
                                  beta = lm(RET.USD~WC09302)$coefficient[2],
                                  no.obs = length(Id)),
                              by = ym]
# NOOO

#value4: Market Capitalization/Common Equity
# 09721 Market Capitalization/Common Equity -no
# 08001 Market Capitalization -YES
# 03501 Common Equity -YES
if('WC08001' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}
if('WC03501' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}
panel_country$WC03501[1:100]
panel_country$WC08001[1:100]
#BM <- Common EquityWC03501/Market CapitalizationWC08001
panel_country$BM <- panel_country$WC03501/panel_country$WC08001

if('BM' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}

newtable_value4 <- panel_country[,.(BM, RET.USD, ym, Id)]
newtable_value4 <- newtable_value4[!is.na(BM) & !is.na(RET.USD) & !is.infinite(BM) & !is.infinite(RET.USD)]

reg.value4 <- newtable_value4[, .(intercept = lm(RET.USD~BM)$coefficient[1],
                                  beta = lm(RET.USD~BM)$coefficient[2],
                                  no.obs = length(Id)),
                              by = ym]

reg.value4[,t.test(intercept)]
reg.value4[,t.test(beta)]


# Annual Statistics
# 
# 09104 Price/Earnings Ratio - Close -no
# 09100 Price/Earnings Ratio - High -no
# 09101 Price/Earnings Ratio - Low -no
# 09106 Price/Earnings Ratio - Avg High-Low -no
# Five Year Averages
# 09321 Price/Book Value Ratio - Close -no
# 09311 Price/Book Value Ratio - High -no
# 09316 Price/Book Value Ratio - Low -no
# 09326 Price/Book Value Ratio - Avg High-Low -no
# 09721 Market Capitalization/Common Equity -no
# 

if('WC09104' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}

# Quality  value_5
# Return on Equity
# Current Information
# 08372 Return on Equity - per Share - Current - no
# Annual Statistics
# 08301 Return on Equity - Total (%) -yes
# 08371 Return on Equity - Per Share -yes
# 08302 Return on Equity – Per Share Fiscal -no
# 
if('WC08301' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}

if('WC08302' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}

newtable_value5 <- panel_country[,.(WC08301, RET.USD, ym, Id)]
newtable_value5 <- newtable_value5[!is.na(WC08301) & !is.na(RET.USD) & !is.infinite(WC08301) & !is.infinite(RET.USD)]

reg.value5 <- newtable_value5[, .(intercept = lm(RET.USD~WC08301)$coefficient[1],
                                  beta = lm(RET.USD~WC08301)$coefficient[2],
                                  no.obs = length(Id)),
                              by = ym]
reg.value5[,t.test(intercept)]
reg.value5[,t.test(beta)]

# Five Year Averages
# 08305 Return on Equity – Total (%)
# 08375 Return on Equity - per Share


#value6 DTE
# Debt to Equity
# 03255 Total Debt -YES
# 08231 Total Debt % Common Equity -NO
# 03501 Common Equity -YES

if('WC08231' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}

if('WC03501' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}

panel_country$DTE <- panel_country$WC03255/panel_country$WC03501

if('DTE' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}

newtable_value6 <- panel_country[,.(DTE, RET.USD, ym, Id)]
newtable_value6 <- newtable_value6[!is.na(DTE) & !is.na(RET.USD) & !is.infinite(DTE) & !is.infinite(RET.USD)]

reg.value6 <- newtable_value6[, .(intercept = lm(RET.USD~DTE)$coefficient[1],
                                  beta = lm(RET.USD~DTE)$coefficient[2],
                                  no.obs = length(Id)),
                              by = ym]

reg.value6[,t.test(intercept)]
reg.value6[,t.test(beta)]


#value 7
# Earnings Variability
# 08601 Earnings per Share Growth
if('WC08601' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}

#value 8 ROA
#08326 Return on Assets 
if('WC08326' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}
newtable_value8 <- panel_country[,.(WC08326, RET.USD, ym, Id)]
newtable_value8 <- newtable_value8[!is.na(WC08326) & !is.na(RET.USD) & !is.infinite(WC08326) & !is.infinite(RET.USD)]

reg.value8 <- newtable_value8[, .(intercept = lm(RET.USD~WC08326)$coefficient[1],
                                  beta = lm(RET.USD~WC08326)$coefficient[2],
                                  no.obs = length(Id)),
                              by = ym]

reg.value8[,t.test(intercept)]
reg.value8[,t.test(beta)]


#value 9 $DTET
# 03255 Total Debt -YES
#06798 Total Equity - GAAP
if('WC06798' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}
panel_country$DTET <- panel_country$WC03255/panel_country$WC06798

if('DTET' %in% colnames(panel_country)){
  print('Yes')
}else{
  print('No')
}

newtable_value9 <- panel_country[,.(DTET, RET.USD, ym, Id)]
newtable_value9 <- newtable_value9[!is.na(DTET) & !is.na(RET.USD) & !is.infinite(DTET) & !is.infinite(RET.USD)]

reg.value9 <- newtable_value9[, .(intercept = lm(RET.USD~DTET)$coefficient[1],
                                  beta = lm(RET.USD~DTET)$coefficient[2],
                                  no.obs = length(Id)),
                              by = ym]

reg.value9[,t.test(intercept)]
reg.value9[,t.test(beta)]

sum(is.infinite(newtable_value9$DTET))
sum(is.infinite(newtable_value9$RET.USD))

# 05301 Common Shares Outstanding
# 08006 Trading Volume ($ Amount)

panel_country$turnover <- panel_country$WC05301/panel_country$W08006

newtable_value10 <- panel_country[,.(turnover, RET.USD, ym, Id)]
newtable_value10 <- newtable_value10[!is.na(turnover) & !is.na(RET.USD) & !is.infinite(turnover) & !is.infinite(RET.USD)]

reg.value10 <- newtable_value10[, .(intercept = lm(RET.USD~turnover)$coefficient[1],
                                  beta = lm(RET.USD~turnover)$coefficient[2],
                                  no.obs = length(Id)),
                              by = ym]

reg.value10[,t.test(intercept)]
reg.value10[,t.test(beta)]

# 05202 Earnings per Share - Fiscal Year End
# Historical sales per share growth rate
# 05508 Sales per Share



newtable_value11 <- panel_country[,.(WC05202,WC05508, RET.USD, ym, Id)]
newtable_value11 <- newtable_value11[!is.na(WC05508) & !is.na(WC05202) & !is.na(RET.USD) & !is.infinite(WC05202) & !is.infinite(RET.USD)]

reg.value11 <- newtable_value11[, .(intercept = lm(RET.USD~WC05202+WC05508)$coefficient[1],
                                    beta1 = lm(RET.USD~WC05202+WC05508)$coefficient[2],
                                    beta2 = lm(RET.USD~WC05202+WC05508)$coefficient[3],
                                    no.obs = length(Id)),
                                by = ym]

reg.value11[,t.test(intercept)]
reg.value11[,t.test(beta1)]
reg.value11[,t.test(beta2)]
   