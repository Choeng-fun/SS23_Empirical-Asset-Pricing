library(data.table)
library(dplyr)
library(lubridate)
library(zoo)

setwd("M:\\Advanced Seminar")
load("Data_Excursus2a.RData")
load("Data_Excursus2b.RData")
load("Data_Excursus2c.RData")

# Regress some of the test portfolios on the CAPM and FF3FM
summary(lm(Data_Excursus2a[,S1V4] ~ Data_Excursus2c[,RMRF] ))
summary(lm(Data_Excursus2a[,S4V4] ~ Data_Excursus2c[,RMRF] ))
summary(lm(Data_Excursus2a[,S1V1] ~ Data_Excursus2c[,RMRF] ))
summary(lm(Data_Excursus2a[,S4V1] ~ Data_Excursus2c[,RMRF] ))

cor.test(Data_Excursus2c[,SMB],Data_Excursus2c[,RMRF])
cor.test(Data_Excursus2c[,SMB],Data_Excursus2c[,HML])

summary(lm(Data_Excursus2a[,S1V4] ~ Data_Excursus2c[,RMRF] + Data_Excursus2c[,SMB] + Data_Excursus2c[,HML] ))
summary(lm(Data_Excursus2a[,S4V4] ~ Data_Excursus2c[,RMRF] + Data_Excursus2c[,SMB] + Data_Excursus2c[,HML] ))
summary(lm(Data_Excursus2a[,S1V1] ~ Data_Excursus2c[,RMRF] + Data_Excursus2c[,SMB] + Data_Excursus2c[,HML] ))
summary(lm(Data_Excursus2a[,S4V1] ~ Data_Excursus2c[,RMRF] + Data_Excursus2c[,SMB] + Data_Excursus2c[,HML] ))

# Regress Momentum quintile returns (UMD) on FF3FM and FF4FM
summary(lm(Data_Excursus2b[,UMD] ~ Data_Excursus2c[,RMRF] + Data_Excursus2c[,SMB] + Data_Excursus2c[,HML] ))
summary(lm(Data_Excursus2b[,UMD] ~ Data_Excursus2c[,RMRF] + Data_Excursus2c[,SMB] + Data_Excursus2c[,HML] + Data_Excursus2c[,WML] ))
