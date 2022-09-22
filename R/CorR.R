# CorR.R
# file to calclulate the correlations for assets
library(quantmod)
library(PerformanceAnalytics)
myAssets <- c("SPY", "TLT", "GLD", 'DBC',"VGSLX", "BTC-USD", "JNK", "EEM")
getSymbols(Symbols = myAssets, from = "2000-01-01")

mydata <- cbind(SPY$SPY.Adjusted, TLT$TLT.Adjusted, GLD$GLD.Adjusted, 
                VGSLX$VGSLX.Adjusted, JNK$JNK.Adjusted, EEM$EEM.Adjusted) 
head(mydata)
tail(mydata)
# Save the data so don't need to continually download from Yahoo
write.csv(mydata, '../Data/mydata.csv')
index(mydata)
da <- read.csv('../Data/mydata.csv', col.names = 
                 c("Date", "SPY", "TLT", "GLD", "VGSLX"))
str(da)
head(da)
da$Date <- index(mydata)
myreturns <- CalculateReturns(mydata, method = 'log')
colnames(myreturns) <- c("SPYR", "TLTR", "GLDR", "VGSLXR", "JNKR", "EEMR")
head(myreturns)
tail(myreturns)
str(myreturns)
#  Use the method of AssetAll.R from line 22 that gets the symbols and 
# assesses performance
# We will look at the correlation and the rolling correlation over time. 
#=================================================
# Correlation in s smaller structure. 
min_ind <- which.max(!is.na(rowSums(myreturns)))
cor(myreturns[min_ind:nrow(myreturns)])
min_ind
cor(myreturns["2020-01-01/2021-12/31"])
plot(rollapply(myreturns[, c("SPYR", "TLTR")], width = 30, FUN = cor), 
     by.column = FALSE)
head(myreturns)
tail(myreturns)
str(myreturns)
head(myreturns[, c("SPYR", "TLTR")])
tail(myreturns[, c("SPYR", "TLTR")])
#=====================================================
myreturns <- as.data.frame(myreturns)


