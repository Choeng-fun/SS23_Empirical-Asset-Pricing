library(data.table)
library(dplyr)
library(lubridate)
library(zoo)

setwd("M:\\Advanced Seminar")
load("Data_Excursus3a.RData")
load("Data_Excursus3b.RData")
load("Data_Excursus3c.RData")



## Spanning Tests
# Check if WML is spanned by the FF3FM, to see if WML can be added to the model
Data_Excursus3a[,summary(lm(WML ~ RMRF.local + SMB + HML))]
# Check if WML is spanned by the FF5FM
Data_Excursus3a[,summary(lm(WML ~ RMRF.local + SMB + HML + CMA + RMW))]
# Check if WML is spanned by the FF3FM with HML_d
Data_Excursus3a[,summary(lm(WML ~ RMRF.local + SMB + HML.devil))]
Data_Excursus3a[,cor.test(WML,HML.devil)]
# The intercept is always sig different from 0. so we can add WML

# Check if Value is spanned by FF3F + WML, to see if it's redundant
Data_Excursus3a[,summary(lm(HML ~ RMRF.local + SMB + WML))]
Data_Excursus3a[,summary(lm(HML.devil ~ RMRF.local + SMB + WML))]
# Intercept is sig different from 0. so not redundant 

## Merging monthly market data with annual accounting information
Data_Excursus3b
Data_Excursus3c

# create a help column to merge the accounting data
# from july in year Y on, use accounting information from year Y-1
Data_Excursus3c[,month := month(Date)]
Data_Excursus3c[,year := year(Date)]
Data_Excursus3c[,hcdec := ifelse(month>=7,year-1,year-2)]
# only after 07.01 we have the accounting data of last year

panel_country <- merge(Data_Excursus3b,Data_Excursus3c,
                       by.x=c("Id","YEAR"),
                       by.y=c("Id","hcdec"),
                       all.x=T # no impact here: yearly panel covers all obs. of monthly panel 
)
# validate the merge
panel_country[year==2015 & month %in% c(6,7) & Id=="13410M"]
# in June 2015, we use accounting data in 2013, in July 2015, we use accounting data in 2014
Data_Excursus3b[YEAR==2013 & Id=="13410M"]
Data_Excursus3b[YEAR==2014 & Id=="13410M"]