library(data.table)
library(dplyr)
library(lubridate)
library(zoo)

setwd("~/Desktop/Empirical Asset Pricing")

# Market beta regression
# construct value-weighted market
load("Data_Excursus1.Rdata")
load("Data_Excursus2c.Rdata")

# define easier-to-handle date format
Data_Excursus1[,ym:=as.yearmon(Date)] 
Data_Excursus2c[,ym:=as.yearmon(Date)]

# Market beta regression
# construct value-weighted market
Data_Excursus <-merge(Data_Excursus1[,-c("Date")],
                      Data_Excursus2c,by="ym")

# ADIDAS AG
# time series analysis to get beta

#Data_866013 <- Data_Excursus[Id=="866013"]

Data_Excursus[Id=="866013",lm(RET.USD~RMRF)]
Data_Excursus[Id=="866013",cov(RET.USD,RMRF)/var(RMRF)]

Data_Excursus[Id=="866013" & ym>=as.yearmon("Jul 2000") & ym<=as.yearmon("Jun 2005"),lm(RET.USD~RMRF)]
Data_Excursus[Id=="866013" & ym>=as.yearmon("Jul 2005") & ym<=as.yearmon("Jun 2010"),lm(RET.USD~RMRF)]


# Fama-MacBeth regression for 07/1990
Data_Excursus1[,ym:=as.yearmon(Date)] # define easier-to-handle date format
Data_Excursus1[ym=="Jul 1990", summary(lm(RET.USD~BETA))] # cross sectional
Data_Excursus1[Date==as.Date("1990-07-31"), summary(lm(RET.USD~BETA))] # same result

# Fama-MacBeth regression for every single month
# Analogous to "for"-loop
CS.reg.estimtates <- Data_Excursus1[, .(gamma_zero=lm(RET.USD~BETA)$coefficient[1], # intercept alpha
                                        gamma=lm(RET.USD~BETA)$coefficient[2], # coefficient for beta
                                        no.obs=length(Id)),by=ym] # FMB directly accounted for FMB (and thereby for degrees of freedom)

View(CS.reg.estimtates)
CS.reg.estimtates[,t.test(gamma_zero)]
CS.reg.estimtates[,t.test(gamma)]

# Fama-MacBeth regression incl. controls
CS.reg.estimtates.contr.1 <- Data_Excursus1[, .(gamma_zero=lm(RET.USD~BETA+LMV.USD)$coefficient[1],
                                                gamma=lm(RET.USD~BETA+LMV.USD)$coefficient[2],
                                                gamma.LMV=lm(RET.USD~BETA+LMV.USD)$coefficient[3],
                                                no.obs=length(Id)),by=ym]

CS.reg.estimtates.contr.1[,t.test(gamma_zero)]
CS.reg.estimtates.contr.1[,t.test(gamma)]
CS.reg.estimtates.contr.1[,t.test(gamma.LMV)]

CS.reg.estimtates.contr.2 <- Data_Excursus1[!is.na(EOJ_Marketcap) & !is.infinite(BM),
                                            .(gamma_zero=lm(RET.USD~BETA+EOJ_Marketcap+BM)$coefficient[1],
                                              gamma=lm(RET.USD~BETA+EOJ_Marketcap+BM)$coefficient[2],
                                              gamma.size=lm(RET.USD~BETA+EOJ_Marketcap+BM)$coefficient[3],
                                              gamma.value=lm(RET.USD~BETA+EOJ_Marketcap+BM)$coefficient[4],
                                              no.obs=length(Id)),by=ym]


CS.reg.estimtates.contr.2[,t.test(gamma_zero)]
CS.reg.estimtates.contr.2[,t.test(gamma)]
CS.reg.estimtates.contr.2[,t.test(gamma.size)]
CS.reg.estimtates.contr.2[,t.test(gamma.value)]